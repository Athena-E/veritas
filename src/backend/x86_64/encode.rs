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

#[derive(Clone, Debug)]
pub struct EncodedProgram {
    pub code: Vec<u8>,
    pub symbols: HashMap<String, usize>,
    pub relocations: Vec<Relocation>,
}

#[derive(Clone, Debug)]
pub struct Relocation {
    pub offset: usize,
    pub target: String,
    pub kind: RelocKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RelocKind {
    Rel32,
}

pub struct Encoder {
    code: Vec<u8>,
    labels: HashMap<String, usize>,
    forward_refs: Vec<(String, usize, RelocKind)>,
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

    pub fn encode_program(&mut self, program: &X86Program) -> EncodedProgram {
        let mut symbols = HashMap::new();
        let mut all_forward_refs: Vec<(String, usize, RelocKind)> = Vec::new();

        for func in &program.functions {
            symbols.insert(func.name.clone(), self.code.len());
            self.func_offset = self.code.len();
            self.labels.clear();
            self.forward_refs.clear();

            self.collect_labels(func);

            for instr in &func.instructions {
                self.encode_instruction(instr);
            }

            self.resolve_forward_refs();

            all_forward_refs.append(&mut self.forward_refs);
        }

        for (target, patch_offset, _kind) in &all_forward_refs {
            if let Some(&target_pos) = symbols.get(target) {
                let offset = (target_pos as i64) - (*patch_offset as i64 + 4);
                self.patch_i32(*patch_offset, offset as i32);
            }
        }

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

    fn collect_labels(&mut self, func: &X86Function) {
        let mut offset = 0;

        for instr in &func.instructions {
            if let X86Instr::Label { name } = instr {
                self.labels.insert(name.clone(), self.func_offset + offset);
            }
            offset += self.instruction_size(instr);
        }
    }

    fn instruction_size(&self, instr: &X86Instr) -> usize {
        match instr {
            X86Instr::Label { .. } | X86Instr::Comment { .. } => 0,

            X86Instr::MovRR { dst, src } => {
                let rex = Self::needs_rex_w() || dst.needs_rex_r() || src.needs_rex_b();
                if rex { 3 } else { 2 }
            }

            X86Instr::MovRI { dst, imm: _ } => {
                let rex = Self::needs_rex_w() || dst.needs_rex_b();
                if rex { 1 + 1 + 8 } else { 1 + 8 }
            }

            X86Instr::MovRM { dst, src } | X86Instr::MovMR { dst: src, src: dst } => {
                self.mem_instr_size(*dst, src)
            }

            X86Instr::MovMI { dst, .. } => self.mem_only_instr_size(dst) + 4,

            X86Instr::Lea { dst, src } => self.mem_instr_size(*dst, src),

            X86Instr::AddRR { .. }
            | X86Instr::SubRR { .. }
            | X86Instr::CmpRR { .. }
            | X86Instr::TestRR { .. }
            | X86Instr::AndRR { .. }
            | X86Instr::OrRR { .. }
            | X86Instr::XorRR { .. } => 3,

            X86Instr::ImulRR { .. } => 4,

            X86Instr::AddRI { dst, imm }
            | X86Instr::SubRI { dst, imm }
            | X86Instr::CmpRI { lhs: dst, imm }
            | X86Instr::AndRI { dst, imm }
            | X86Instr::OrRI { dst, imm }
            | X86Instr::XorRI { dst, imm }
            | X86Instr::TestRI { lhs: dst, imm } => {
                let rex = Self::needs_rex_w() || dst.needs_rex_b();
                let base = if rex { 3 } else { 2 };
                if *imm >= -128 && *imm <= 127 && !matches!(instr, X86Instr::TestRI { .. }) {
                    base + 1
                } else {
                    base + 4
                }
            }

            X86Instr::ImulRRI { .. } => 7,

            X86Instr::AddRM { dst, src } => self.mem_instr_size(*dst, src),
            X86Instr::SubRM { dst, src } => self.mem_instr_size(*dst, src),
            X86Instr::CmpRM { lhs, rhs } => self.mem_instr_size(*lhs, rhs),

            X86Instr::Cqo => 2,
            X86Instr::IdivR { src: _ } => 3,

            X86Instr::Neg { .. } | X86Instr::Not { .. } => 3,
            X86Instr::ShlCl { .. } | X86Instr::ShrCl { .. } => 3,
            X86Instr::ShlRI { .. } | X86Instr::ShrRI { .. } => 4,

            X86Instr::SetCC { dst, .. } => {
                let setcc_size = if dst.needs_rex_for_byte() { 4 } else { 3 };
                let movzx_size = if dst.needs_rex_for_byte() { 4 } else { 3 };
                setcc_size + movzx_size
            }

            X86Instr::Jmp { .. } | X86Instr::JmpRel { .. } => 5,
            X86Instr::Jcc { .. } | X86Instr::JccRel { .. } => 6,
            X86Instr::Call { .. } | X86Instr::CallRel { .. } => 5,

            X86Instr::Ret => 1,
            X86Instr::Syscall => 2,
            X86Instr::InAlDx => 1,
            X86Instr::OutDxAl => 1,

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

    fn mem_instr_size(&self, _reg: X86Reg, mem: &MemOperand) -> usize {
        let mut size = 3;

        if mem.index.is_some() || mem.base == X86Reg::Rsp || mem.base == X86Reg::R12 {
            size += 1;
        }

        if mem.disp == 0 && mem.base != X86Reg::Rbp && mem.base != X86Reg::R13 {
        } else if mem.disp >= -128 && mem.disp <= 127 {
            size += 1;
        } else {
            size += 4;
        }

        size
    }

    fn mem_only_instr_size(&self, mem: &MemOperand) -> usize {
        let mut size = 3;

        if mem.index.is_some() || mem.base == X86Reg::Rsp || mem.base == X86Reg::R12 {
            size += 1;
        }

        if mem.disp == 0 && mem.base != X86Reg::Rbp && mem.base != X86Reg::R13 {
        } else if mem.disp >= -128 && mem.disp <= 127 {
            size += 1;
        } else {
            size += 4;
        }

        size
    }

    fn encode_instruction(&mut self, instr: &X86Instr) {
        match instr {
            X86Instr::Label { name } => {
                self.labels.insert(name.clone(), self.code.len());
            }

            X86Instr::Comment { .. } => {}

            X86Instr::MovRR { dst, src } => {
                self.encode_rr(0x89, *src, *dst);
            }

            X86Instr::MovRI { dst, imm } => {
                let rex = 0x48 | if dst.needs_rex_b() { 0x01 } else { 0 };
                self.emit_byte(rex);
                self.emit_byte(0xB8 + dst.reg3());
                self.emit_i64(*imm);
            }

            X86Instr::MovRM { dst, src } => {
                self.encode_rm(0x8B, *dst, src);
            }

            X86Instr::MovMR { dst, src } => {
                self.encode_mr(0x89, dst, *src);
            }

            X86Instr::MovMI { dst, imm } => {
                self.encode_mi(0xC7, 0, dst, *imm);
            }

            X86Instr::Lea { dst, src } => {
                self.encode_rm(0x8D, *dst, src);
            }

            X86Instr::AddRR { dst, src } => {
                self.encode_rr(0x01, *src, *dst);
            }

            X86Instr::AddRI { dst, imm } => {
                self.encode_ri(0x81, 0x83, 0, *dst, *imm);
            }

            X86Instr::AddRM { dst, src } => {
                self.encode_rm(0x03, *dst, src);
            }

            X86Instr::SubRR { dst, src } => {
                self.encode_rr(0x29, *src, *dst);
            }

            X86Instr::SubRI { dst, imm } => {
                self.encode_ri(0x81, 0x83, 5, *dst, *imm);
            }

            X86Instr::SubRM { dst, src } => {
                self.encode_rm(0x2B, *dst, src);
            }

            X86Instr::ImulRR { dst, src } => {
                let rex = 0x48
                    | if dst.needs_rex_r() { 0x04 } else { 0 }
                    | if src.needs_rex_b() { 0x01 } else { 0 };
                self.emit_byte(rex);
                self.emit_byte(0x0F);
                self.emit_byte(0xAF);
                self.emit_modrm(0b11, dst.reg3(), src.reg3());
            }

            X86Instr::ImulRRI { dst, src, imm } => {
                let rex = 0x48
                    | if dst.needs_rex_r() { 0x04 } else { 0 }
                    | if src.needs_rex_b() { 0x01 } else { 0 };
                self.emit_byte(rex);
                self.emit_byte(0x69);
                self.emit_modrm(0b11, dst.reg3(), src.reg3());
                self.emit_i32(*imm);
            }

            X86Instr::Cqo => {
                self.emit_byte(0x48);
                self.emit_byte(0x99);
            }

            X86Instr::IdivR { src } => {
                self.encode_unary(0xF7, 7, *src);
            }

            X86Instr::Neg { dst } => {
                self.encode_unary(0xF7, 3, *dst);
            }

            X86Instr::CmpRR { lhs, rhs } => {
                self.encode_rr(0x39, *rhs, *lhs);
            }

            X86Instr::CmpRI { lhs, imm } => {
                self.encode_ri(0x81, 0x83, 7, *lhs, *imm);
            }

            X86Instr::CmpRM { lhs, rhs } => {
                self.encode_rm(0x3B, *lhs, rhs);
            }

            X86Instr::TestRR { lhs, rhs } => {
                self.encode_rr(0x85, *rhs, *lhs);
            }

            X86Instr::TestRI { lhs, imm } => {
                let rex = 0x48 | if lhs.needs_rex_b() { 0x01 } else { 0 };
                self.emit_byte(rex);
                self.emit_byte(0xF7);
                self.emit_modrm(0b11, 0, lhs.reg3());
                self.emit_i32(*imm);
            }

            X86Instr::SetCC { dst, cond } => {
                if dst.needs_rex_for_byte() {
                    let rex = 0x40 | if dst.needs_rex_b() { 0x01 } else { 0 };
                    self.emit_byte(rex);
                }
                self.emit_byte(0x0F);
                self.emit_byte(cond.setcc_byte());
                self.emit_modrm(0b11, 0, dst.reg3());

                if dst.needs_rex_for_byte() {
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
                self.encode_rr(0x21, *src, *dst);
            }

            X86Instr::AndRI { dst, imm } => {
                self.encode_ri(0x81, 0x83, 4, *dst, *imm);
            }

            X86Instr::OrRR { dst, src } => {
                self.encode_rr(0x09, *src, *dst);
            }

            X86Instr::OrRI { dst, imm } => {
                self.encode_ri(0x81, 0x83, 1, *dst, *imm);
            }

            X86Instr::XorRR { dst, src } => {
                self.encode_rr(0x31, *src, *dst);
            }

            X86Instr::XorRI { dst, imm } => {
                self.encode_ri(0x81, 0x83, 6, *dst, *imm);
            }

            X86Instr::Not { dst } => {
                self.encode_unary(0xF7, 2, *dst);
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

            X86Instr::ShlCl { dst } => {
                self.encode_unary(0xD3, 4, *dst);
            }

            X86Instr::ShrCl { dst } => {
                self.encode_unary(0xD3, 5, *dst);
            }

            X86Instr::ShlRI { dst, imm } => {
                self.encode_unary(0xC1, 4, *dst);
                self.emit_byte(*imm);
            }

            X86Instr::ShrRI { dst, imm } => {
                self.encode_unary(0xC1, 5, *dst);
                self.emit_byte(*imm);
            }

            X86Instr::InAlDx => {
                self.emit_byte(0xEC);
            }

            X86Instr::OutDxAl => {
                self.emit_byte(0xEE);
            }
        }
    }

    fn encode_rr(&mut self, opcode: u8, reg: X86Reg, rm: X86Reg) {
        let rex = 0x48
            | if reg.needs_rex_r() { 0x04 } else { 0 }
            | if rm.needs_rex_b() { 0x01 } else { 0 };
        self.emit_byte(rex);
        self.emit_byte(opcode);
        self.emit_modrm(0b11, reg.reg3(), rm.reg3());
    }

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

    fn encode_mr(&mut self, opcode: u8, mem: &MemOperand, reg: X86Reg) {
        self.encode_rm(opcode, reg, mem);
    }

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

    fn encode_unary(&mut self, opcode: u8, ext: u8, dst: X86Reg) {
        let rex = 0x48 | if dst.needs_rex_b() { 0x01 } else { 0 };
        self.emit_byte(rex);
        self.emit_byte(opcode);
        self.emit_modrm(0b11, ext, dst.reg3());
    }

    fn encode_mem_operand(&mut self, reg: u8, mem: &MemOperand) {
        let base = mem.base;
        let base3 = base.reg3();

        let (mod_bits, disp_size) = if mem.disp == 0 && base != X86Reg::Rbp && base != X86Reg::R13 {
            (0b00, 0)
        } else if mem.disp >= -128 && mem.disp <= 127 {
            (0b01, 1)
        } else {
            (0b10, 4)
        };

        if let Some((index, scale)) = mem.index {
            let scale_bits = match scale {
                1 => 0b00,
                2 => 0b01,
                4 => 0b10,
                8 => 0b11,
                _ => panic!("Invalid scale: {}", scale),
            };

            self.emit_modrm(mod_bits, reg, 0b100);
            self.emit_sib(scale_bits, index.reg3(), base3);
        } else if base == X86Reg::Rsp || base == X86Reg::R12 {
            self.emit_modrm(mod_bits, reg, 0b100);
            self.emit_sib(0b00, 0b100, base3);
        } else {
            self.emit_modrm(mod_bits, reg, base3);
        }

        match disp_size {
            0 => {}
            1 => self.emit_byte(mem.disp as u8),
            4 => self.emit_i32(mem.disp),
            _ => unreachable!(),
        }
    }

    fn encode_jmp_label(&mut self, target: &str) {
        let current_pos = self.code.len();

        if let Some(&target_pos) = self.labels.get(target) {
            let offset = (target_pos as i64) - (current_pos as i64 + 5);
            self.emit_byte(0xE9);
            self.emit_i32(offset as i32);
        } else {
            self.emit_byte(0xE9);
            self.forward_refs
                .push((target.to_string(), self.code.len(), RelocKind::Rel32));
            self.emit_i32(0);
        }
    }

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

        self.forward_refs
            .retain(|(target, _, _)| !self.labels.contains_key(target));
    }

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

        assert_eq!(&encoded.code[0..3], &[0x48, 0x89, 0xD8]);
        assert_eq!(encoded.code[3], 0xC3);
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

        assert_eq!(encoded.code[0], 0x55);
        assert_eq!(&encoded.code[1..3], &[0x41, 0x54]);
        assert_eq!(&encoded.code[3..5], &[0x41, 0x5C]);
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

        assert_eq!(&encoded.code[0..2], &[0x48, 0x01]);

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

        assert_eq!(encoded.code[0], 0xE9);

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

        assert_eq!(&encoded.code[0..2], &[0x0F, 0x05]);
        assert_eq!(encoded.code[2], 0xC3);
    }
}
