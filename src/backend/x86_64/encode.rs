//! x86-64 Instruction Encoding
//!
//! This module encodes x86-64 instructions into machine code bytes.
//!
//! # Encoding Format
//!
//! x86-64 instructions have the general format:
//! ```text
//! [Prefixes] [REX] [Opcode] [ModR/M] [SIB] [Displacement] [Immediate]
//! ```

use super::instr::{Condition, MemOperand, X86Function, X86Instr, X86Program};
use super::regs::X86Reg;
use std::collections::HashMap;

/// Encoded machine code with label information
#[derive(Clone, Debug)]
pub struct EncodedProgram {
    /// Raw machine code bytes
    pub code: Vec<u8>,
    /// Symbol table: name -> offset
    pub symbols: HashMap<String, usize>,
    /// Relocations: offset -> target label
    pub relocations: Vec<Relocation>,
}

/// A relocation entry
#[derive(Clone, Debug)]
pub struct Relocation {
    /// Offset in code where the relocation applies
    pub offset: usize,
    /// Target symbol name
    pub target: String,
    /// Type of relocation
    pub kind: RelocKind,
}

/// Relocation types
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RelocKind {
    /// PC-relative 32-bit (for near jumps/calls)
    Rel32,
}

/// x86-64 instruction encoder
pub struct Encoder {
    /// Output buffer
    code: Vec<u8>,
    /// Label positions
    labels: HashMap<String, usize>,
    /// Forward references (label, patch_offset, is_call)
    forward_refs: Vec<(String, usize, RelocKind)>,
    /// Current function offset
    func_offset: usize,
}

impl Encoder {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            labels: HashMap::new(),
            forward_refs: Vec::new(),
            func_offset: 0,
        }
    }

    /// Encode a complete program
    pub fn encode_program(&mut self, program: &X86Program) -> EncodedProgram {
        let mut symbols = HashMap::new();
        let mut all_forward_refs: Vec<(String, usize, RelocKind)> = Vec::new();

        for func in &program.functions {
            // Record function symbol
            symbols.insert(func.name.clone(), self.code.len());
            self.func_offset = self.code.len();
            self.labels.clear();
            self.forward_refs.clear();

            // First pass: collect labels
            self.collect_labels(func);

            // Second pass: encode instructions
            for instr in &func.instructions {
                self.encode_instruction(instr);
            }

            // Resolve forward references within function
            self.resolve_forward_refs();

            // Accumulate unresolved references for cross-function resolution
            all_forward_refs.append(&mut self.forward_refs);
        }

        // Resolve cross-function references using the global symbols table
        for (target, patch_offset, _kind) in &all_forward_refs {
            if let Some(&target_pos) = symbols.get(target) {
                let offset = (target_pos as i64) - (*patch_offset as i64 + 4);
                self.patch_i32(*patch_offset, offset as i32);
            }
        }

        // Build relocations for truly unresolved references (external symbols)
        let relocations: Vec<Relocation> = all_forward_refs
            .iter()
            .filter(|(target, _, _)| !symbols.contains_key(target))
            .map(|(target, offset, kind)| Relocation {
                offset: *offset,
                target: target.clone(),
                kind: *kind,
            })
            .collect();

        EncodedProgram {
            code: self.code.clone(),
            symbols,
            relocations,
        }
    }

    /// Collect label positions (first pass)
    fn collect_labels(&mut self, func: &X86Function) {
        let mut offset = 0;

        for instr in &func.instructions {
            if let X86Instr::Label { name } = instr {
                self.labels.insert(name.clone(), self.func_offset + offset);
            }
            offset += self.instruction_size(instr);
        }
    }

    /// Calculate instruction size without encoding
    fn instruction_size(&self, instr: &X86Instr) -> usize {
        match instr {
            X86Instr::Label { .. } | X86Instr::Comment { .. } => 0,

            X86Instr::MovRR { dst, src } => {
                let rex = Self::needs_rex_w() || dst.needs_rex_r() || src.needs_rex_b();
                if rex { 3 } else { 2 }
            }

            X86Instr::MovRI { dst, imm: _ } => {
                let rex = Self::needs_rex_w() || dst.needs_rex_b();
                // REX + opcode + imm64
                if rex { 1 + 1 + 8 } else { 1 + 8 }
            }

            X86Instr::MovRM { dst, src } | X86Instr::MovMR { dst: src, src: dst } => {
                self.mem_instr_size(*dst, src)
            }

            X86Instr::MovMI { dst, .. } => {
                // REX.W + opcode + ModR/M + SIB? + disp + imm32
                self.mem_only_instr_size(dst) + 4
            }

            X86Instr::Lea { dst, src } => self.mem_instr_size(*dst, src),

            X86Instr::AddRR { .. }
            | X86Instr::SubRR { .. }
            | X86Instr::ImulRR { .. }
            | X86Instr::CmpRR { .. }
            | X86Instr::TestRR { .. }
            | X86Instr::AndRR { .. }
            | X86Instr::OrRR { .. }
            | X86Instr::XorRR { .. } => 3, // REX.W + opcode + ModR/M

            X86Instr::AddRI { dst, imm }
            | X86Instr::SubRI { dst, imm }
            | X86Instr::CmpRI { lhs: dst, imm }
            | X86Instr::AndRI { dst, imm }
            | X86Instr::OrRI { dst, imm }
            | X86Instr::XorRI { dst, imm }
            | X86Instr::TestRI { lhs: dst, imm } => {
                let rex = Self::needs_rex_w() || dst.needs_rex_b();
                let base = if rex { 3 } else { 2 };
                // Check if imm fits in i8
                if *imm >= -128 && *imm <= 127 && !matches!(instr, X86Instr::TestRI { .. }) {
                    base + 1
                } else {
                    base + 4
                }
            }

            X86Instr::ImulRRI { .. } => 7, // REX.W + 0x69 + ModR/M + imm32

            X86Instr::AddRM { .. } | X86Instr::SubRM { .. } | X86Instr::CmpRM { .. } => {
                // Estimate: REX + opcode + ModR/M + SIB? + disp
                7
            }

            X86Instr::Neg { .. } | X86Instr::Not { .. } => 3, // REX.W + opcode + ModR/M

            X86Instr::SetCC { dst, .. } => {
                // setcc r8 (3-4 bytes) + movzx r32, r8 (3-4 bytes)
                // Using movzx instead of xor to avoid clobbering flags
                let setcc_size = if dst.needs_rex_b() { 4 } else { 3 };
                let movzx_size = if dst.needs_rex_r() || dst.needs_rex_b() {
                    4
                } else {
                    3
                };
                setcc_size + movzx_size
            }

            X86Instr::Jmp { .. } | X86Instr::JmpRel { .. } => 5, // E9 + rel32
            X86Instr::Jcc { .. } | X86Instr::JccRel { .. } => 6, // 0F 8x + rel32
            X86Instr::Call { .. } | X86Instr::CallRel { .. } => 5, // E8 + rel32

            X86Instr::Ret => 1,
            X86Instr::Syscall => 2,

            X86Instr::Push { src } => {
                if src.needs_rex_b() {
                    2
                } else {
                    1
                }
            }
            X86Instr::PushI { imm } => {
                if *imm >= -128 && *imm <= 127 {
                    2
                } else {
                    5
                }
            }
            X86Instr::Pop { dst } => {
                if dst.needs_rex_b() {
                    2
                } else {
                    1
                }
            }
        }
    }

    /// Calculate size of memory instruction
    fn mem_instr_size(&self, _reg: X86Reg, mem: &MemOperand) -> usize {
        let mut size = 3; // REX.W + opcode + ModR/M

        // SIB byte needed?
        if mem.index.is_some() || mem.base == X86Reg::Rsp || mem.base == X86Reg::R12 {
            size += 1;
        }

        // Displacement size
        if mem.disp == 0 && mem.base != X86Reg::Rbp && mem.base != X86Reg::R13 {
            // No displacement
        } else if mem.disp >= -128 && mem.disp <= 127 {
            size += 1;
        } else {
            size += 4;
        }

        size
    }

    /// Calculate size of memory-only instruction (e.g., MovMI)
    fn mem_only_instr_size(&self, mem: &MemOperand) -> usize {
        let mut size = 3; // REX.W + opcode + ModR/M

        if mem.index.is_some() || mem.base == X86Reg::Rsp || mem.base == X86Reg::R12 {
            size += 1;
        }

        if mem.disp == 0 && mem.base != X86Reg::Rbp && mem.base != X86Reg::R13 {
            // No displacement
        } else if mem.disp >= -128 && mem.disp <= 127 {
            size += 1;
        } else {
            size += 4;
        }

        size
    }

    /// Encode a single instruction
    fn encode_instruction(&mut self, instr: &X86Instr) {
        match instr {
            X86Instr::Label { name } => {
                // Labels don't emit code, position already recorded
                self.labels.insert(name.clone(), self.code.len());
            }

            X86Instr::Comment { .. } => {
                // Comments don't emit code
            }

            X86Instr::MovRR { dst, src } => {
                self.encode_rr(0x89, *src, *dst); // mov r/m64, r64
            }

            X86Instr::MovRI { dst, imm } => {
                // movabs r64, imm64
                let rex = 0x48 | if dst.needs_rex_b() { 0x01 } else { 0 };
                self.emit_byte(rex);
                self.emit_byte(0xB8 + dst.reg3());
                self.emit_i64(*imm);
            }

            X86Instr::MovRM { dst, src } => {
                self.encode_rm(0x8B, *dst, src); // mov r64, r/m64
            }

            X86Instr::MovMR { dst, src } => {
                self.encode_mr(0x89, dst, *src); // mov r/m64, r64
            }

            X86Instr::MovMI { dst, imm } => {
                // mov r/m64, imm32 (sign-extended)
                self.encode_mi(0xC7, 0, dst, *imm);
            }

            X86Instr::Lea { dst, src } => {
                self.encode_rm(0x8D, *dst, src); // lea r64, m
            }

            X86Instr::AddRR { dst, src } => {
                self.encode_rr(0x01, *src, *dst); // add r/m64, r64
            }

            X86Instr::AddRI { dst, imm } => {
                self.encode_ri(0x81, 0x83, 0, *dst, *imm); // add r/m64, imm
            }

            X86Instr::AddRM { dst, src } => {
                self.encode_rm(0x03, *dst, src); // add r64, r/m64
            }

            X86Instr::SubRR { dst, src } => {
                self.encode_rr(0x29, *src, *dst); // sub r/m64, r64
            }

            X86Instr::SubRI { dst, imm } => {
                self.encode_ri(0x81, 0x83, 5, *dst, *imm); // sub r/m64, imm
            }

            X86Instr::SubRM { dst, src } => {
                self.encode_rm(0x2B, *dst, src); // sub r64, r/m64
            }

            X86Instr::ImulRR { dst, src } => {
                // imul r64, r/m64 (two operand form)
                let rex = 0x48
                    | if dst.needs_rex_r() { 0x04 } else { 0 }
                    | if src.needs_rex_b() { 0x01 } else { 0 };
                self.emit_byte(rex);
                self.emit_byte(0x0F);
                self.emit_byte(0xAF);
                self.emit_modrm(0b11, dst.reg3(), src.reg3());
            }

            X86Instr::ImulRRI { dst, src, imm } => {
                // imul r64, r/m64, imm32
                let rex = 0x48
                    | if dst.needs_rex_r() { 0x04 } else { 0 }
                    | if src.needs_rex_b() { 0x01 } else { 0 };
                self.emit_byte(rex);
                self.emit_byte(0x69);
                self.emit_modrm(0b11, dst.reg3(), src.reg3());
                self.emit_i32(*imm);
            }

            X86Instr::Neg { dst } => {
                self.encode_unary(0xF7, 3, *dst); // neg r/m64
            }

            X86Instr::CmpRR { lhs, rhs } => {
                self.encode_rr(0x39, *rhs, *lhs); // cmp r/m64, r64
            }

            X86Instr::CmpRI { lhs, imm } => {
                self.encode_ri(0x81, 0x83, 7, *lhs, *imm); // cmp r/m64, imm
            }

            X86Instr::CmpRM { lhs, rhs } => {
                self.encode_rm(0x3B, *lhs, rhs); // cmp r64, r/m64
            }

            X86Instr::TestRR { lhs, rhs } => {
                self.encode_rr(0x85, *rhs, *lhs); // test r/m64, r64
            }

            X86Instr::TestRI { lhs, imm } => {
                // test r/m64, imm32 (no short form)
                let rex = 0x48 | if lhs.needs_rex_b() { 0x01 } else { 0 };
                self.emit_byte(rex);
                self.emit_byte(0xF7);
                self.emit_modrm(0b11, 0, lhs.reg3());
                self.emit_i32(*imm);
            }

            X86Instr::SetCC { dst, cond } => {
                // setcc r8 - sets low byte based on condition
                // Encoding: [REX.B if needed] 0x0F 0x9x ModR/M
                if dst.needs_rex_b() {
                    self.emit_byte(0x41);
                }
                self.emit_byte(0x0F);
                self.emit_byte(cond.setcc_byte());
                self.emit_modrm(0b11, 0, dst.reg3());

                // movzx r32, r8 - zero-extend to 32-bit (implicitly zeros upper 32 bits)
                // This preserves the flags from the prior cmp instruction
                // Encoding: [REX if needed] 0x0F 0xB6 ModR/M
                if dst.needs_rex_r() || dst.needs_rex_b() {
                    let rex = 0x40
                        | if dst.needs_rex_r() { 0x04 } else { 0 }
                        | if dst.needs_rex_b() { 0x01 } else { 0 };
                    self.emit_byte(rex);
                }
                self.emit_byte(0x0F);
                self.emit_byte(0xB6);
                self.emit_modrm(0b11, dst.reg3(), dst.reg3());
            }

            X86Instr::AndRR { dst, src } => {
                self.encode_rr(0x21, *src, *dst); // and r/m64, r64
            }

            X86Instr::AndRI { dst, imm } => {
                self.encode_ri(0x81, 0x83, 4, *dst, *imm); // and r/m64, imm
            }

            X86Instr::OrRR { dst, src } => {
                self.encode_rr(0x09, *src, *dst); // or r/m64, r64
            }

            X86Instr::OrRI { dst, imm } => {
                self.encode_ri(0x81, 0x83, 1, *dst, *imm); // or r/m64, imm
            }

            X86Instr::XorRR { dst, src } => {
                self.encode_rr(0x31, *src, *dst); // xor r/m64, r64
            }

            X86Instr::XorRI { dst, imm } => {
                self.encode_ri(0x81, 0x83, 6, *dst, *imm); // xor r/m64, imm
            }

            X86Instr::Not { dst } => {
                self.encode_unary(0xF7, 2, *dst); // not r/m64
            }

            X86Instr::Jmp { target } => {
                self.encode_jmp_label(target);
            }

            X86Instr::JmpRel { offset } => {
                self.emit_byte(0xE9);
                self.emit_i32(*offset);
            }

            X86Instr::Jcc { cond, target } => {
                self.encode_jcc_label(*cond, target);
            }

            X86Instr::JccRel { cond, offset } => {
                self.emit_byte(0x0F);
                self.emit_byte(cond.cc_byte());
                self.emit_i32(*offset);
            }

            X86Instr::Call { target } => {
                self.encode_call_label(target);
            }

            X86Instr::CallRel { offset } => {
                self.emit_byte(0xE8);
                self.emit_i32(*offset);
            }

            X86Instr::Ret => {
                self.emit_byte(0xC3);
            }

            X86Instr::Push { src } => {
                if src.needs_rex_b() {
                    self.emit_byte(0x41);
                }
                self.emit_byte(0x50 + src.reg3());
            }

            X86Instr::PushI { imm } => {
                if *imm >= -128 && *imm <= 127 {
                    self.emit_byte(0x6A);
                    self.emit_byte(*imm as u8);
                } else {
                    self.emit_byte(0x68);
                    self.emit_i32(*imm);
                }
            }

            X86Instr::Pop { dst } => {
                if dst.needs_rex_b() {
                    self.emit_byte(0x41);
                }
                self.emit_byte(0x58 + dst.reg3());
            }

            X86Instr::Syscall => {
                self.emit_byte(0x0F);
                self.emit_byte(0x05);
            }
        }
    }

    /// Encode reg-reg instruction
    fn encode_rr(&mut self, opcode: u8, reg: X86Reg, rm: X86Reg) {
        let rex = 0x48
            | if reg.needs_rex_r() { 0x04 } else { 0 }
            | if rm.needs_rex_b() { 0x01 } else { 0 };
        self.emit_byte(rex);
        self.emit_byte(opcode);
        self.emit_modrm(0b11, reg.reg3(), rm.reg3());
    }

    /// Encode reg-mem instruction
    fn encode_rm(&mut self, opcode: u8, reg: X86Reg, mem: &MemOperand) {
        let rex = 0x48
            | if reg.needs_rex_r() { 0x04 } else { 0 }
            | if mem.base.needs_rex_b() { 0x01 } else { 0 }
            | if mem.index.is_some_and(|(r, _)| r.needs_rex_b()) {
                0x02
            } else {
                0
            };
        self.emit_byte(rex);
        self.emit_byte(opcode);
        self.encode_mem_operand(reg.reg3(), mem);
    }

    /// Encode mem-reg instruction
    fn encode_mr(&mut self, opcode: u8, mem: &MemOperand, reg: X86Reg) {
        self.encode_rm(opcode, reg, mem);
    }

    /// Encode mem-imm instruction
    fn encode_mi(&mut self, opcode: u8, ext: u8, mem: &MemOperand, imm: i32) {
        let rex = 0x48
            | if mem.base.needs_rex_b() { 0x01 } else { 0 }
            | if mem.index.is_some_and(|(r, _)| r.needs_rex_b()) {
                0x02
            } else {
                0
            };
        self.emit_byte(rex);
        self.emit_byte(opcode);
        self.encode_mem_operand(ext, mem);
        self.emit_i32(imm);
    }

    /// Encode reg-imm instruction
    fn encode_ri(&mut self, opcode32: u8, opcode8: u8, ext: u8, dst: X86Reg, imm: i32) {
        let rex = 0x48 | if dst.needs_rex_b() { 0x01 } else { 0 };
        self.emit_byte(rex);

        if (-128..=127).contains(&imm) {
            self.emit_byte(opcode8);
            self.emit_modrm(0b11, ext, dst.reg3());
            self.emit_byte(imm as u8);
        } else {
            self.emit_byte(opcode32);
            self.emit_modrm(0b11, ext, dst.reg3());
            self.emit_i32(imm);
        }
    }

    /// Encode unary instruction (neg, not, etc.)
    fn encode_unary(&mut self, opcode: u8, ext: u8, dst: X86Reg) {
        let rex = 0x48 | if dst.needs_rex_b() { 0x01 } else { 0 };
        self.emit_byte(rex);
        self.emit_byte(opcode);
        self.emit_modrm(0b11, ext, dst.reg3());
    }

    /// Encode memory operand (ModR/M + SIB + displacement)
    fn encode_mem_operand(&mut self, reg: u8, mem: &MemOperand) {
        let base = mem.base;
        let base3 = base.reg3();

        // Determine mod and displacement size
        let (mod_bits, disp_size) = if mem.disp == 0 && base != X86Reg::Rbp && base != X86Reg::R13 {
            (0b00, 0)
        } else if mem.disp >= -128 && mem.disp <= 127 {
            (0b01, 1)
        } else {
            (0b10, 4)
        };

        if let Some((index, scale)) = mem.index {
            // SIB byte needed
            let scale_bits = match scale {
                1 => 0b00,
                2 => 0b01,
                4 => 0b10,
                8 => 0b11,
                _ => panic!("Invalid scale: {}", scale),
            };

            self.emit_modrm(mod_bits, reg, 0b100); // SIB follows
            self.emit_sib(scale_bits, index.reg3(), base3);
        } else if base == X86Reg::Rsp || base == X86Reg::R12 {
            // RSP/R12 as base requires SIB
            self.emit_modrm(mod_bits, reg, 0b100);
            self.emit_sib(0b00, 0b100, base3); // No index
        } else {
            self.emit_modrm(mod_bits, reg, base3);
        }

        // Emit displacement
        match disp_size {
            0 => {}
            1 => self.emit_byte(mem.disp as u8),
            4 => self.emit_i32(mem.disp),
            _ => unreachable!(),
        }
    }

    /// Encode jump to label
    fn encode_jmp_label(&mut self, target: &str) {
        let current_pos = self.code.len();

        if let Some(&target_pos) = self.labels.get(target) {
            // Backward reference - calculate offset
            let offset = (target_pos as i64) - (current_pos as i64 + 5);
            self.emit_byte(0xE9);
            self.emit_i32(offset as i32);
        } else {
            // Forward reference - emit placeholder
            self.emit_byte(0xE9);
            self.forward_refs
                .push((target.to_string(), self.code.len(), RelocKind::Rel32));
            self.emit_i32(0); // Placeholder
        }
    }

    /// Encode conditional jump to label
    fn encode_jcc_label(&mut self, cond: Condition, target: &str) {
        let current_pos = self.code.len();

        if let Some(&target_pos) = self.labels.get(target) {
            let offset = (target_pos as i64) - (current_pos as i64 + 6);
            self.emit_byte(0x0F);
            self.emit_byte(cond.cc_byte());
            self.emit_i32(offset as i32);
        } else {
            self.emit_byte(0x0F);
            self.emit_byte(cond.cc_byte());
            self.forward_refs
                .push((target.to_string(), self.code.len(), RelocKind::Rel32));
            self.emit_i32(0);
        }
    }

    /// Encode call to label
    fn encode_call_label(&mut self, target: &str) {
        let current_pos = self.code.len();

        if let Some(&target_pos) = self.labels.get(target) {
            let offset = (target_pos as i64) - (current_pos as i64 + 5);
            self.emit_byte(0xE8);
            self.emit_i32(offset as i32);
        } else {
            self.emit_byte(0xE8);
            self.forward_refs
                .push((target.to_string(), self.code.len(), RelocKind::Rel32));
            self.emit_i32(0);
        }
    }

    /// Resolve forward references within function
    fn resolve_forward_refs(&mut self) {
        let resolved: Vec<_> = self
            .forward_refs
            .iter()
            .filter_map(|(target, patch_offset, _kind)| {
                self.labels.get(target).map(|&target_pos| {
                    let offset = (target_pos as i64) - (*patch_offset as i64 + 4);
                    (*patch_offset, offset as i32)
                })
            })
            .collect();

        for (patch_offset, offset) in resolved {
            self.patch_i32(patch_offset, offset);
        }

        // Remove resolved references
        self.forward_refs
            .retain(|(target, _, _)| !self.labels.contains_key(target));
    }

    /// Check if REX.W is needed (always true for 64-bit operations)
    fn needs_rex_w() -> bool {
        true
    }

    fn emit_byte(&mut self, b: u8) {
        self.code.push(b);
    }

    fn emit_modrm(&mut self, mod_bits: u8, reg: u8, rm: u8) {
        self.code.push((mod_bits << 6) | (reg << 3) | rm);
    }

    fn emit_sib(&mut self, scale: u8, index: u8, base: u8) {
        self.code.push((scale << 6) | (index << 3) | base);
    }

    fn emit_i32(&mut self, v: i32) {
        self.code.extend_from_slice(&v.to_le_bytes());
    }

    fn emit_i64(&mut self, v: i64) {
        self.code.extend_from_slice(&v.to_le_bytes());
    }

    fn patch_i32(&mut self, offset: usize, v: i32) {
        let bytes = v.to_le_bytes();
        self.code[offset..offset + 4].copy_from_slice(&bytes);
    }
}

impl Default for Encoder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::x86_64::instr::X86Function;

    #[test]
    fn test_encode_ret() {
        let func = X86Function {
            name: "test".to_string(),
            instructions: vec![X86Instr::Ret],
        };

        let program = X86Program {
            functions: vec![func],
        };

        let mut encoder = Encoder::new();
        let encoded = encoder.encode_program(&program);

        assert_eq!(encoded.code, vec![0xC3]);
    }

    #[test]
    fn test_encode_mov_rr() {
        let func = X86Function {
            name: "test".to_string(),
            instructions: vec![
                X86Instr::MovRR {
                    dst: X86Reg::Rax,
                    src: X86Reg::Rbx,
                },
                X86Instr::Ret,
            ],
        };

        let program = X86Program {
            functions: vec![func],
        };

        let mut encoder = Encoder::new();
        let encoded = encoder.encode_program(&program);

        // REX.W (0x48) + MOV (0x89) + ModR/M (0xC3 = mod=11, reg=rbx, r/m=rax)
        assert_eq!(&encoded.code[0..3], &[0x48, 0x89, 0xD8]);
        assert_eq!(encoded.code[3], 0xC3); // ret
    }

    #[test]
    fn test_encode_push_pop() {
        let func = X86Function {
            name: "test".to_string(),
            instructions: vec![
                X86Instr::Push { src: X86Reg::Rbp },
                X86Instr::Push { src: X86Reg::R12 },
                X86Instr::Pop { dst: X86Reg::R12 },
                X86Instr::Pop { dst: X86Reg::Rbp },
                X86Instr::Ret,
            ],
        };

        let program = X86Program {
            functions: vec![func],
        };

        let mut encoder = Encoder::new();
        let encoded = encoder.encode_program(&program);

        // push rbp = 0x55
        assert_eq!(encoded.code[0], 0x55);
        // push r12 = 0x41 0x54
        assert_eq!(&encoded.code[1..3], &[0x41, 0x54]);
        // pop r12 = 0x41 0x5C
        assert_eq!(&encoded.code[3..5], &[0x41, 0x5C]);
        // pop rbp = 0x5D
        assert_eq!(encoded.code[5], 0x5D);
    }

    #[test]
    fn test_encode_add_sub() {
        let func = X86Function {
            name: "test".to_string(),
            instructions: vec![
                X86Instr::AddRR {
                    dst: X86Reg::Rax,
                    src: X86Reg::Rbx,
                },
                X86Instr::SubRI {
                    dst: X86Reg::Rsp,
                    imm: 32,
                },
                X86Instr::Ret,
            ],
        };

        let program = X86Program {
            functions: vec![func],
        };

        let mut encoder = Encoder::new();
        let encoded = encoder.encode_program(&program);

        // add rax, rbx = REX.W (0x48) + 0x01 + ModR/M
        assert_eq!(&encoded.code[0..2], &[0x48, 0x01]);

        // Verify we got some valid encoding
        assert!(encoded.code.len() > 5);
    }

    #[test]
    fn test_encode_jmp_forward() {
        let func = X86Function {
            name: "test".to_string(),
            instructions: vec![
                X86Instr::Jmp {
                    target: "end".to_string(),
                },
                X86Instr::MovRI {
                    dst: X86Reg::Rax,
                    imm: 42,
                },
                X86Instr::Label {
                    name: "end".to_string(),
                },
                X86Instr::Ret,
            ],
        };

        let program = X86Program {
            functions: vec![func],
        };

        let mut encoder = Encoder::new();
        let encoded = encoder.encode_program(&program);

        // First byte should be JMP (0xE9)
        assert_eq!(encoded.code[0], 0xE9);

        // Last byte should be RET
        assert_eq!(*encoded.code.last().unwrap(), 0xC3);
    }

    #[test]
    fn test_encode_syscall() {
        let func = X86Function {
            name: "test".to_string(),
            instructions: vec![X86Instr::Syscall, X86Instr::Ret],
        };

        let program = X86Program {
            functions: vec![func],
        };

        let mut encoder = Encoder::new();
        let encoded = encoder.encode_program(&program);

        // syscall = 0x0F 0x05
        assert_eq!(&encoded.code[0..2], &[0x0F, 0x05]);
        assert_eq!(encoded.code[2], 0xC3);
    }
}
