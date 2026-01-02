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

/// ELF64 file header
const ELF_MAGIC: [u8; 4] = [0x7F, b'E', b'L', b'F'];

/// ELF class
const ELFCLASS64: u8 = 2;

/// ELF data encoding (little endian)
const ELFDATA2LSB: u8 = 1;

/// ELF version
const EV_CURRENT: u8 = 1;

/// ELF OS/ABI (System V)
const ELFOSABI_NONE: u8 = 0;

/// ELF type (executable)
const ET_EXEC: u16 = 2;

/// ELF machine (x86-64)
const EM_X86_64: u16 = 62;

/// Program header type (loadable segment)
const PT_LOAD: u32 = 1;

/// Segment flags
const PF_X: u32 = 1; // Execute
#[allow(dead_code)]
const PF_W: u32 = 2; // Write
const PF_R: u32 = 4; // Read

/// Default virtual address for code segment
const CODE_VADDR: u64 = 0x400000;

/// ELF64 header size
const ELF64_EHDR_SIZE: u16 = 64;

/// Program header size
const ELF64_PHDR_SIZE: u16 = 56;

/// ELF generator
pub struct ElfGenerator {
    /// Virtual address for entry point
    entry_point: u64,
    /// Code section content
    code: Vec<u8>,
    /// Symbol table
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

    /// Set the entry point symbol
    pub fn set_entry(&mut self, symbol: &str, encoded: &EncodedProgram) {
        if let Some(&offset) = encoded.symbols.get(symbol) {
            // Entry point is virtual address + offset in code
            self.entry_point =
                CODE_VADDR + ELF64_EHDR_SIZE as u64 + ELF64_PHDR_SIZE as u64 + offset as u64;
        }
    }

    /// Load encoded program
    pub fn load_program(&mut self, encoded: &EncodedProgram) {
        self.code = encoded.code.clone();

        // Calculate virtual addresses for symbols
        let code_base = CODE_VADDR + ELF64_EHDR_SIZE as u64 + ELF64_PHDR_SIZE as u64;
        for (name, &offset) in &encoded.symbols {
            self.symbols.insert(name.clone(), code_base + offset as u64);
        }
    }

    /// Generate ELF file
    pub fn generate<W: Write>(&self, out: &mut W) -> io::Result<()> {
        // Calculate sizes
        let header_size = ELF64_EHDR_SIZE as usize + ELF64_PHDR_SIZE as usize;
        let total_size = header_size + self.code.len();

        // Write ELF header
        self.write_elf_header(out, total_size)?;

        // Write program header
        self.write_program_header(out, total_size)?;

        // Write code
        out.write_all(&self.code)?;

        Ok(())
    }

    /// Write ELF64 header
    fn write_elf_header<W: Write>(&self, out: &mut W, _file_size: usize) -> io::Result<()> {
        // e_ident
        out.write_all(&ELF_MAGIC)?; // Magic
        out.write_all(&[ELFCLASS64])?; // Class (64-bit)
        out.write_all(&[ELFDATA2LSB])?; // Data (little endian)
        out.write_all(&[EV_CURRENT])?; // Version
        out.write_all(&[ELFOSABI_NONE])?; // OS/ABI
        out.write_all(&[0; 8])?; // Padding

        // e_type
        out.write_all(&ET_EXEC.to_le_bytes())?;

        // e_machine
        out.write_all(&EM_X86_64.to_le_bytes())?;

        // e_version
        out.write_all(&1u32.to_le_bytes())?;

        // e_entry (entry point virtual address)
        out.write_all(&self.entry_point.to_le_bytes())?;

        // e_phoff (program header offset)
        out.write_all(&(ELF64_EHDR_SIZE as u64).to_le_bytes())?;

        // e_shoff (section header offset - 0 for minimal executable)
        out.write_all(&0u64.to_le_bytes())?;

        // e_flags
        out.write_all(&0u32.to_le_bytes())?;

        // e_ehsize (ELF header size)
        out.write_all(&ELF64_EHDR_SIZE.to_le_bytes())?;

        // e_phentsize (program header entry size)
        out.write_all(&ELF64_PHDR_SIZE.to_le_bytes())?;

        // e_phnum (number of program headers)
        out.write_all(&1u16.to_le_bytes())?;

        // e_shentsize (section header entry size)
        out.write_all(&0u16.to_le_bytes())?;

        // e_shnum (number of section headers)
        out.write_all(&0u16.to_le_bytes())?;

        // e_shstrndx (section name string table index)
        out.write_all(&0u16.to_le_bytes())?;

        Ok(())
    }

    /// Write program header for code segment
    fn write_program_header<W: Write>(&self, out: &mut W, file_size: usize) -> io::Result<()> {
        // p_type (PT_LOAD)
        out.write_all(&PT_LOAD.to_le_bytes())?;

        // p_flags (readable and executable)
        out.write_all(&(PF_R | PF_X).to_le_bytes())?;

        // p_offset (file offset)
        out.write_all(&0u64.to_le_bytes())?;

        // p_vaddr (virtual address)
        out.write_all(&CODE_VADDR.to_le_bytes())?;

        // p_paddr (physical address - same as vaddr for our purposes)
        out.write_all(&CODE_VADDR.to_le_bytes())?;

        // p_filesz (size in file)
        out.write_all(&(file_size as u64).to_le_bytes())?;

        // p_memsz (size in memory)
        out.write_all(&(file_size as u64).to_le_bytes())?;

        // p_align (alignment)
        out.write_all(&0x1000u64.to_le_bytes())?;

        Ok(())
    }

    /// Generate a standalone executable that calls the entry function and exits
    pub fn generate_standalone<W: Write>(&self, out: &mut W, entry_symbol: &str) -> io::Result<()> {
        // For a standalone executable, we need to:
        // 1. Set up the stack
        // 2. Call the main function
        // 3. Use the return value as exit code
        // 4. Call exit syscall

        // Build startup code
        let mut startup: Vec<u8> = Vec::new();

        // call main function (will be patched)
        startup.push(0xE8); // call rel32
        startup.extend_from_slice(&[0, 0, 0, 0]); // placeholder

        // mov rdi, rax (return value becomes exit code)
        startup.extend_from_slice(&[0x48, 0x89, 0xC7]);

        // mov rax, 60 (exit syscall number)
        startup.extend_from_slice(&[0x48, 0xC7, 0xC0, 60, 0, 0, 0]);

        // syscall
        startup.extend_from_slice(&[0x0F, 0x05]);

        let startup_size = startup.len();

        // Patch the call offset
        if let Some(symbol_offset) = self
            .symbols
            .get(entry_symbol)
            .map(|v| v - CODE_VADDR - ELF64_EHDR_SIZE as u64 - ELF64_PHDR_SIZE as u64)
        {
            let call_offset = (symbol_offset as i64 + startup_size as i64 - 5) as i32;
            startup[1..5].copy_from_slice(&call_offset.to_le_bytes());
        }

        // Combine startup and program code
        let mut full_code = startup;
        full_code.extend_from_slice(&self.code);

        // Calculate sizes
        let header_size = ELF64_EHDR_SIZE as usize + ELF64_PHDR_SIZE as usize;
        let total_size = header_size + full_code.len();

        // Entry point is at start of code (after headers)
        let entry_point = CODE_VADDR + header_size as u64;

        // Write headers with updated entry point
        self.write_elf_header_with_entry(out, total_size, entry_point)?;
        self.write_program_header(out, total_size)?;

        // Write code
        out.write_all(&full_code)?;

        Ok(())
    }

    /// Write ELF header with custom entry point
    fn write_elf_header_with_entry<W: Write>(
        &self,
        out: &mut W,
        _file_size: usize,
        entry: u64,
    ) -> io::Result<()> {
        // e_ident
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

/// Generate a minimal ELF executable from encoded program
pub fn generate_elf(encoded: &EncodedProgram, entry: &str) -> Vec<u8> {
    let mut generator = ElfGenerator::new();
    generator.load_program(encoded);
    generator.set_entry(entry, encoded);

    let mut output = Vec::new();
    generator
        .generate_standalone(&mut output, entry)
        .expect("ELF generation failed");
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_elf_header_size() {
        // ELF64 header should be 64 bytes
        assert_eq!(ELF64_EHDR_SIZE, 64);
    }

    #[test]
    fn test_generate_minimal_elf() {
        // Create a minimal program that just returns
        let encoded = EncodedProgram {
            code: vec![
                // mov rax, 42
                0x48, 0xC7, 0xC0, 42, 0, 0, 0, // ret
                0xC3,
            ],
            symbols: {
                let mut s = HashMap::new();
                s.insert("main".to_string(), 0);
                s
            },
            relocations: vec![],
        };

        let elf = generate_elf(&encoded, "main");

        // Check ELF magic
        assert_eq!(&elf[0..4], &ELF_MAGIC);

        // Check it's 64-bit
        assert_eq!(elf[4], ELFCLASS64);

        // Check it's little endian
        assert_eq!(elf[5], ELFDATA2LSB);

        // Check it's executable type
        let e_type = u16::from_le_bytes([elf[16], elf[17]]);
        assert_eq!(e_type, ET_EXEC);

        // Check machine type
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

        // The code should appear somewhere after the headers
        // (with startup code prepended)
        let header_size = ELF64_EHDR_SIZE as usize + ELF64_PHDR_SIZE as usize;

        // ELF should be larger than just headers
        assert!(elf.len() > header_size);

        // The original code should be present (after startup wrapper)
        let code_section = &elf[header_size..];
        assert!(code_section.windows(2).any(|w| w == [0x0F, 0x05])); // syscall in startup
    }
}
