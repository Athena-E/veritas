//! ELF Generation
//!
//! This module generates ELF64 executables for Linux x86-64.
//!
//! # ELF Structure
//!
//! ```text
//! ┌─────────────────────┐
//! │    ELF Header       │
//! ├─────────────────────┤
//! │  Program Headers    │
//! ├─────────────────────┤
//! │    .text section    │  (executable code)
//! ├─────────────────────┤
//! │   Section Headers   │  (optional for execution)
//! └─────────────────────┘
//! ```

use crate::backend::x86_64::encode::EncodedProgram;
use std::collections::HashMap;
use std::io::{self, Write};

const ELF_MAGIC: [u8; 4] = [0x7F, b'E', b'L', b'F'];

const ELFCLASS64: u8 = 2;

const ELFDATA2LSB: u8 = 1;

const EV_CURRENT: u8 = 1;

const ELFOSABI_NONE: u8 = 0;

const ET_EXEC: u16 = 2;

const EM_X86_64: u16 = 62;

const PT_LOAD: u32 = 1;

#[allow(dead_code)]
const PF_X: u32 = 1;
const PF_W: u32 = 2;
const PF_R: u32 = 4;

const CODE_VADDR: u64 = 0x400000;

const ELF64_EHDR_SIZE: u16 = 64;

const ELF64_PHDR_SIZE: u16 = 56;

pub struct ElfGenerator {
    entry_point: u64,
    code: Vec<u8>,
    symbols: HashMap<String, u64>,
}

impl ElfGenerator {
    pub fn new() -> Self {
        Self {
            entry_point: CODE_VADDR,
            code: Vec::new(),
            symbols: HashMap::new(),
        }
    }

    pub fn set_entry(&mut self, symbol: &str, encoded: &EncodedProgram) {
        if let Some(&offset) = encoded.symbols.get(symbol) {
            self.entry_point =
                CODE_VADDR + ELF64_EHDR_SIZE as u64 + ELF64_PHDR_SIZE as u64 + offset as u64;
        }
    }

    pub fn load_program(&mut self, encoded: &EncodedProgram) {
        self.code = encoded.code.clone();

        let code_base = CODE_VADDR + ELF64_EHDR_SIZE as u64 + ELF64_PHDR_SIZE as u64;
        for (name, &offset) in &encoded.symbols {
            self.symbols.insert(name.clone(), code_base + offset as u64);
        }
    }

    pub fn generate<W: Write>(&self, out: &mut W) -> io::Result<()> {
        let header_size = ELF64_EHDR_SIZE as usize + ELF64_PHDR_SIZE as usize;
        let total_size = header_size + self.code.len();

        self.write_elf_header(out, total_size)?;
        self.write_program_header(out, total_size)?;
        out.write_all(&self.code)?;

        Ok(())
    }

    fn write_elf_header<W: Write>(&self, out: &mut W, _file_size: usize) -> io::Result<()> {
        out.write_all(&ELF_MAGIC)?;
        out.write_all(&[ELFCLASS64])?;
        out.write_all(&[ELFDATA2LSB])?;
        out.write_all(&[EV_CURRENT])?;
        out.write_all(&[ELFOSABI_NONE])?;
        out.write_all(&[0; 8])?;

        out.write_all(&ET_EXEC.to_le_bytes())?;
        out.write_all(&EM_X86_64.to_le_bytes())?;
        out.write_all(&1u32.to_le_bytes())?;
        out.write_all(&self.entry_point.to_le_bytes())?;
        out.write_all(&(ELF64_EHDR_SIZE as u64).to_le_bytes())?;
        out.write_all(&0u64.to_le_bytes())?;
        out.write_all(&0u32.to_le_bytes())?;
        out.write_all(&ELF64_EHDR_SIZE.to_le_bytes())?;
        out.write_all(&ELF64_PHDR_SIZE.to_le_bytes())?;
        out.write_all(&1u16.to_le_bytes())?;
        out.write_all(&0u16.to_le_bytes())?;
        out.write_all(&0u16.to_le_bytes())?;
        out.write_all(&0u16.to_le_bytes())?;

        Ok(())
    }

    fn write_program_header<W: Write>(&self, out: &mut W, file_size: usize) -> io::Result<()> {
        out.write_all(&PT_LOAD.to_le_bytes())?;
        out.write_all(&(PF_R | PF_X).to_le_bytes())?;
        out.write_all(&0u64.to_le_bytes())?;
        out.write_all(&CODE_VADDR.to_le_bytes())?;
        out.write_all(&CODE_VADDR.to_le_bytes())?;
        out.write_all(&(file_size as u64).to_le_bytes())?;
        out.write_all(&(file_size as u64).to_le_bytes())?;
        out.write_all(&0x1000u64.to_le_bytes())?;

        Ok(())
    }

    pub fn generate_standalone<W: Write>(&self, out: &mut W, entry_symbol: &str) -> io::Result<()> {
        let mut startup: Vec<u8> = Vec::new();

        startup.push(0xE8);
        startup.extend_from_slice(&[0, 0, 0, 0]);

        startup.extend_from_slice(&[0x48, 0x89, 0xC7]);

        startup.extend_from_slice(&[0x48, 0xC7, 0xC0, 60, 0, 0, 0]);

        startup.extend_from_slice(&[0x0F, 0x05]);

        let startup_size = startup.len();

        if let Some(symbol_offset) = self
            .symbols
            .get(entry_symbol)
            .map(|v| v - CODE_VADDR - ELF64_EHDR_SIZE as u64 - ELF64_PHDR_SIZE as u64)
        {
            let call_offset = (symbol_offset as i64 + startup_size as i64 - 5) as i32;
            startup[1..5].copy_from_slice(&call_offset.to_le_bytes());
        }

        let mut full_code = startup;
        full_code.extend_from_slice(&self.code);

        let header_size = ELF64_EHDR_SIZE as usize + ELF64_PHDR_SIZE as usize;
        let total_size = header_size + full_code.len();
        let entry_point = CODE_VADDR + header_size as u64;

        self.write_elf_header_with_entry(out, total_size, entry_point)?;
        self.write_program_header(out, total_size)?;
        out.write_all(&full_code)?;

        Ok(())
    }

    fn write_elf_header_with_entry<W: Write>(
        &self,
        out: &mut W,
        _file_size: usize,
        entry: u64,
    ) -> io::Result<()> {
        out.write_all(&ELF_MAGIC)?;
        out.write_all(&[ELFCLASS64])?;
        out.write_all(&[ELFDATA2LSB])?;
        out.write_all(&[EV_CURRENT])?;
        out.write_all(&[ELFOSABI_NONE])?;
        out.write_all(&[0; 8])?;

        out.write_all(&ET_EXEC.to_le_bytes())?;
        out.write_all(&EM_X86_64.to_le_bytes())?;
        out.write_all(&1u32.to_le_bytes())?;
        out.write_all(&entry.to_le_bytes())?;
        out.write_all(&(ELF64_EHDR_SIZE as u64).to_le_bytes())?;
        out.write_all(&0u64.to_le_bytes())?;
        out.write_all(&0u32.to_le_bytes())?;
        out.write_all(&ELF64_EHDR_SIZE.to_le_bytes())?;
        out.write_all(&ELF64_PHDR_SIZE.to_le_bytes())?;
        out.write_all(&1u16.to_le_bytes())?;
        out.write_all(&0u16.to_le_bytes())?;
        out.write_all(&0u16.to_le_bytes())?;
        out.write_all(&0u16.to_le_bytes())?;

        Ok(())
    }
}

impl Default for ElfGenerator {
    fn default() -> Self {
        Self::new()
    }
}

pub fn generate_elf(encoded: &EncodedProgram, entry: &str) -> Vec<u8> {
    use crate::backend::runtime;

    let runtime_blob = runtime::runtime_code();
    let runtime_syms = runtime::runtime_symbols();
    let runtime_size = runtime_blob.len();

    let mut combined_code = runtime_blob;
    combined_code.extend_from_slice(&encoded.code);

    let mut combined_symbols = HashMap::new();
    for (name, &offset) in &encoded.symbols {
        combined_symbols.insert(name.clone(), offset + runtime_size);
    }

    for (name, offset) in &runtime_syms {
        combined_symbols.insert(name.clone(), *offset);
    }

    for reloc in &encoded.relocations {
        if let Some(&target_pos) = combined_symbols.get(&reloc.target) {
            let patch_offset = reloc.offset + runtime_size;
            let offset = (target_pos as i64) - (patch_offset as i64 + 4);
            let bytes = (offset as i32).to_le_bytes();
            combined_code[patch_offset..patch_offset + 4].copy_from_slice(&bytes);
        }
    }

    let combined_encoded = EncodedProgram {
        code: combined_code,
        symbols: combined_symbols,
        relocations: vec![],
    };

    let mut generator = ElfGenerator::new();
    generator.load_program(&combined_encoded);
    generator.set_entry(entry, &combined_encoded);

    let mut output = Vec::new();
    generator
        .generate_standalone(&mut output, entry)
        .expect("ELF generation failed");
    output
}

const ELFCLASS32: u8 = 1;
const EM_386: u16 = 3;
const ELF32_EHDR_SIZE: u16 = 52;
const ELF32_PHDR_SIZE: u16 = 32;
const BAREMETAL_VADDR: u32 = 0x100000;

const BOOTSTRAP_CALL_PATCH_OFFSET: usize = 0xA7;
const BOOTSTRAP_LGDT_ADDR_OFFSET: usize = 0x89;
const BOOTSTRAP_LJMP_ADDR_OFFSET: usize = 0x8E;
const BOOTSTRAP_GDT_OFFSET: usize = 0xB0;
const BOOTSTRAP_GDT_PTR_BASE_OFFSET: usize = 0xCA;
const BOOTSTRAP_LONG_MODE_OFFSET: usize = 0x94;

fn bootstrap_blob() -> Vec<u8> {
    vec![
        0x02, 0xb0, 0xad, 0x1b, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x4f, 0x52, 0xe4, 0xfa, 0xbc, 0x00,
        0x00, 0x20, 0x00, 0xbf, 0x00, 0x10, 0x00, 0x00, 0x31, 0xc0, 0xb9, 0x00, 0x0c, 0x00, 0x00,
        0xf3, 0xab, 0xc7, 0x05, 0x00, 0x10, 0x00, 0x00, 0x03, 0x20, 0x00, 0x00, 0xc7, 0x05, 0x00,
        0x20, 0x00, 0x00, 0x03, 0x30, 0x00, 0x00, 0xc7, 0x05, 0x00, 0x30, 0x00, 0x00, 0x83, 0x00,
        0x00, 0x00, 0xc7, 0x05, 0x08, 0x30, 0x00, 0x00, 0x83, 0x00, 0x20, 0x00, 0xc7, 0x05, 0x10,
        0x30, 0x00, 0x00, 0x83, 0x00, 0x40, 0x00, 0xc7, 0x05, 0x18, 0x30, 0x00, 0x00, 0x83, 0x00,
        0x60, 0x00, 0xb8, 0x00, 0x10, 0x00, 0x00, 0x0f, 0x22, 0xd8, 0x0f, 0x20, 0xe0, 0x83, 0xc8,
        0x20, 0x0f, 0x22, 0xe0, 0xb9, 0x80, 0x00, 0x00, 0xc0, 0x0f, 0x32, 0x0d, 0x00, 0x01, 0x00,
        0x00, 0x0f, 0x30, 0x0f, 0x20, 0xc0, 0x0d, 0x00, 0x00, 0x00, 0x80, 0x0f, 0x22, 0xc0, 0x0f,
        0x01, 0x15, 0xc8, 0x00, 0x00, 0x00, 0xea, 0x94, 0x00, 0x00, 0x00, 0x08, 0x00, 0x66, 0x31,
        0xc0, 0x8e, 0xd8, 0x8e, 0xc0, 0x8e, 0xd0, 0x48, 0xbc, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00,
        0x00, 0x00, 0xe8, 0x00, 0x00, 0x00, 0x00, 0xf4, 0xeb, 0xfd, 0x90, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0x00, 0x00, 0x00, 0x9a, 0xaf, 0x00, 0xff, 0xff, 0x00,
        0x00, 0x00, 0x92, 0xaf, 0x00, 0x17, 0x00, 0xb0, 0x00, 0x00, 0x00,
    ]
}

pub fn generate_baremetal_elf(encoded: &EncodedProgram, entry: &str) -> Vec<u8> {
    use crate::backend::runtime;

    let bootstrap = bootstrap_blob();
    let bootstrap_size = bootstrap.len();

    let runtime_blob = runtime::runtime_code();
    let runtime_syms = runtime::runtime_symbols();
    let runtime_size = runtime_blob.len();

    let mut code = bootstrap;
    code.extend_from_slice(&runtime_blob);
    code.extend_from_slice(&encoded.code);

    let mut symbols: HashMap<String, usize> = HashMap::new();

    for (name, offset) in &runtime_syms {
        symbols.insert(name.clone(), bootstrap_size + offset);
    }

    for (name, &offset) in &encoded.symbols {
        symbols.insert(name.clone(), bootstrap_size + runtime_size + offset);
    }

    if let Some(&main_offset) = symbols.get(entry) {
        let call_addr = BOOTSTRAP_CALL_PATCH_OFFSET;
        let rel = (main_offset as i64) - (call_addr as i64 + 5);
        code[call_addr + 1..call_addr + 5].copy_from_slice(&(rel as i32).to_le_bytes());
    }

    for reloc in &encoded.relocations {
        if let Some(&target_pos) = symbols.get(&reloc.target) {
            let patch_offset = bootstrap_size + runtime_size + reloc.offset;
            let rel = (target_pos as i64) - (patch_offset as i64 + 4);
            code[patch_offset..patch_offset + 4].copy_from_slice(&(rel as i32).to_le_bytes());
        }
    }

    let header_size = ELF32_EHDR_SIZE as usize + ELF32_PHDR_SIZE as usize;
    let code_base = BAREMETAL_VADDR + header_size as u32;

    let gdt_ptr_addr = code_base + BOOTSTRAP_GDT_OFFSET as u32;
    code[BOOTSTRAP_GDT_PTR_BASE_OFFSET..BOOTSTRAP_GDT_PTR_BASE_OFFSET + 4]
        .copy_from_slice(&gdt_ptr_addr.to_le_bytes());
    let gdt_ptr_loc = code_base + 0xC8;
    code[BOOTSTRAP_LGDT_ADDR_OFFSET..BOOTSTRAP_LGDT_ADDR_OFFSET + 4]
        .copy_from_slice(&gdt_ptr_loc.to_le_bytes());
    let long_mode_addr = code_base + BOOTSTRAP_LONG_MODE_OFFSET as u32;
    code[BOOTSTRAP_LJMP_ADDR_OFFSET..BOOTSTRAP_LJMP_ADDR_OFFSET + 4]
        .copy_from_slice(&long_mode_addr.to_le_bytes());

    let total_file_size = header_size + code.len();
    let total_mem_size = total_file_size + 0x10000;
    let entry_point = code_base + 0x0C;

    let mut output = Vec::new();

    output.extend_from_slice(&ELF_MAGIC);
    output.push(ELFCLASS32);
    output.push(ELFDATA2LSB);
    output.push(EV_CURRENT);
    output.push(ELFOSABI_NONE);
    output.extend_from_slice(&[0; 8]);
    output.extend_from_slice(&ET_EXEC.to_le_bytes());
    output.extend_from_slice(&EM_386.to_le_bytes());
    output.extend_from_slice(&1u32.to_le_bytes());
    output.extend_from_slice(&entry_point.to_le_bytes());
    output.extend_from_slice(&(ELF32_EHDR_SIZE as u32).to_le_bytes());
    output.extend_from_slice(&0u32.to_le_bytes());
    output.extend_from_slice(&0u32.to_le_bytes());
    output.extend_from_slice(&ELF32_EHDR_SIZE.to_le_bytes());
    output.extend_from_slice(&ELF32_PHDR_SIZE.to_le_bytes());
    output.extend_from_slice(&1u16.to_le_bytes());
    output.extend_from_slice(&0u16.to_le_bytes());
    output.extend_from_slice(&0u16.to_le_bytes());
    output.extend_from_slice(&0u16.to_le_bytes());

    output.extend_from_slice(&PT_LOAD.to_le_bytes());
    output.extend_from_slice(&0u32.to_le_bytes());
    output.extend_from_slice(&BAREMETAL_VADDR.to_le_bytes());
    output.extend_from_slice(&BAREMETAL_VADDR.to_le_bytes());
    output.extend_from_slice(&(total_file_size as u32).to_le_bytes());
    output.extend_from_slice(&(total_mem_size as u32).to_le_bytes());
    output.extend_from_slice(&(PF_R | PF_W | PF_X).to_le_bytes());
    output.extend_from_slice(&0x1000u32.to_le_bytes());

    output.extend_from_slice(&code);

    output
}
#[cfg(test)]

mod tests {
    use super::*;
    use crate::backend::runtime;
    #[test]

    fn test_elf_header_size() {
        assert_eq!(ELF64_EHDR_SIZE, 64);
    }
    #[test]

    fn test_generate_minimal_elf() {
        let encoded = EncodedProgram {
            code: vec![0x48, 0xC7, 0xC0, 42, 0, 0, 0, 0xC3],
            symbols: {
                let mut s = HashMap::new();
                s.insert("main".to_string(), 0);
                s
            },
            relocations: vec![],
        };

        let elf = generate_elf(&encoded, "main");

        assert_eq!(&elf[0..4], &ELF_MAGIC);
        assert_eq!(elf[4], ELFCLASS64);
        assert_eq!(elf[5], ELFDATA2LSB);

        let e_type = u16::from_le_bytes([elf[16], elf[17]]);
        assert_eq!(e_type, ET_EXEC);

        let e_machine = u16::from_le_bytes([elf[18], elf[19]]);
        assert_eq!(e_machine, EM_X86_64);
    }
    #[test]

    fn test_elf_contains_code() {
        let encoded = EncodedProgram {
            code: vec![0x48, 0xC7, 0xC0, 42, 0, 0, 0, 0xC3],
            symbols: {
                let mut s = HashMap::new();
                s.insert("main".to_string(), 0);
                s
            },
            relocations: vec![],
        };

        let elf = generate_elf(&encoded, "main");

        let header_size = ELF64_EHDR_SIZE as usize + ELF64_PHDR_SIZE as usize;
        assert!(elf.len() > header_size);

        let code_section = &elf[header_size..];
        assert!(code_section.windows(2).any(|w| w == [0x0F, 0x05]));
    }
    #[test]

    fn test_baremetal_bootstrap_call_targets_main_start() {
        let encoded = EncodedProgram {
            code: vec![0x55, 0x48, 0x89, 0xE5, 0xC3],
            symbols: {
                let mut s = HashMap::new();
                s.insert("main".to_string(), 0);
                s
            },
            relocations: vec![],
        };

        let elf = generate_baremetal_elf(&encoded, "main");
        let header_size = ELF32_EHDR_SIZE as usize + ELF32_PHDR_SIZE as usize;
        let code = &elf[header_size..];
        let runtime_size = runtime::runtime_code().len();
        let expected_main_offset = bootstrap_blob().len() + runtime_size;

        assert_eq!(code[BOOTSTRAP_CALL_PATCH_OFFSET], 0xE8);
        let rel = i32::from_le_bytes(
            code[BOOTSTRAP_CALL_PATCH_OFFSET + 1..BOOTSTRAP_CALL_PATCH_OFFSET + 5]
                .try_into()
                .unwrap(),
        );
        let actual_target = (BOOTSTRAP_CALL_PATCH_OFFSET as i64 + 5 + rel as i64) as usize;

        assert_eq!(actual_target, expected_main_offset);
        assert_eq!(code[actual_target], 0x55);
    }
}
