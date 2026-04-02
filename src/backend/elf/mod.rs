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
///
/// Prepends the runtime blob (print_int, print_char, read_int) before
/// user code and resolves call relocations to runtime functions.
pub fn generate_elf(encoded: &EncodedProgram, entry: &str) -> Vec<u8> {
    use crate::backend::runtime;

    let runtime_blob = runtime::runtime_code();
    let runtime_syms = runtime::runtime_symbols();
    let runtime_size = runtime_blob.len();

    // Build a new EncodedProgram with runtime prepended:
    // [runtime_blob | user_code]
    let mut combined_code = runtime_blob;
    combined_code.extend_from_slice(&encoded.code);

    // Shift all user symbols by runtime_size
    let mut combined_symbols = HashMap::new();
    for (name, &offset) in &encoded.symbols {
        combined_symbols.insert(name.clone(), offset + runtime_size);
    }

    // Add runtime symbols (offsets are relative to start of combined code)
    for (name, offset) in &runtime_syms {
        combined_symbols.insert(name.clone(), *offset);
    }

    // Resolve runtime relocations in the user code
    // The user code starts at runtime_size within combined_code,
    // so relocation patch offsets need to be shifted.
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
        relocations: vec![], // all resolved
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

// ============================================================
// Bare-Metal (Multiboot) ELF Generation
// ============================================================

/// ELF32 constants for Multiboot-compatible binary
const ELFCLASS32: u8 = 1;
const EM_386: u16 = 3;
const ELF32_EHDR_SIZE: u16 = 52;
const ELF32_PHDR_SIZE: u16 = 32;
const BAREMETAL_VADDR: u32 = 0x100000;

/// Multiboot bootstrap blob (32→64 bit transition).
/// Assembled from multiboot_bootstrap.s. Trusted code (~182 bytes).
/// Contains: Multiboot header, page table setup, PAE/LME/paging enable,
/// GDT load, far jump to 64-bit, stack setup, call main.
///
/// The `call main` rel32 placeholder is at code offset 0xA8 (opcode 0xE8 at 0xA7).
const BOOTSTRAP_CALL_PATCH_OFFSET: usize = 0xA7;
/// lgdt address operand is at code offset 0x89 (4 bytes referencing gdt_ptr)
const BOOTSTRAP_LGDT_ADDR_OFFSET: usize = 0x89;
/// ljmp target address is at code offset 0x8E (4 bytes referencing long_mode)
const BOOTSTRAP_LJMP_ADDR_OFFSET: usize = 0x8E;
/// GDT is at code offset 0xB0
const BOOTSTRAP_GDT_OFFSET: usize = 0xB0;
/// GDT pointer base field is at code offset 0xCA (4 bytes)
const BOOTSTRAP_GDT_PTR_BASE_OFFSET: usize = 0xCA;
/// long_mode label is at code offset 0x94
const BOOTSTRAP_LONG_MODE_OFFSET: usize = 0x94;

fn bootstrap_blob() -> Vec<u8> {
    // Assembled from boot_v2.s with `as --32`.
    // 206 bytes: Multiboot header + 32→64 bit transition + GDT.
    // Maps first 8MB of physical memory via 4 x 2MB huge pages.
    vec![
        // Multiboot header (12 bytes: magic, flags, checksum)
        0x02, 0xb0, 0xad, 0x1b, 0x00, 0x00, 0x00, 0x00, 0xfe, 0x4f, 0x52, 0xe4,
        // _start: cli; mov esp, 0x200000; zero 3 pages at 0x1000
        0xfa, 0xbc, 0x00, 0x00, 0x20, 0x00, 0xbf, 0x00, 0x10, 0x00, 0x00, 0x31,
        0xc0, 0xb9, 0x00, 0x0c, 0x00, 0x00, 0xf3, 0xab,
        // P4[0]->P3, P3[0]->P2
        0xc7, 0x05, 0x00, 0x10, 0x00, 0x00, 0x03, 0x20, 0x00, 0x00,
        0xc7, 0x05, 0x00, 0x20, 0x00, 0x00, 0x03, 0x30, 0x00, 0x00,
        // P2[0..3]: 4 x 2MB huge pages (8MB identity mapped)
        0xc7, 0x05, 0x00, 0x30, 0x00, 0x00, 0x83, 0x00, 0x00, 0x00,
        0xc7, 0x05, 0x08, 0x30, 0x00, 0x00, 0x83, 0x00, 0x20, 0x00,
        0xc7, 0x05, 0x10, 0x30, 0x00, 0x00, 0x83, 0x00, 0x40, 0x00,
        0xc7, 0x05, 0x18, 0x30, 0x00, 0x00, 0x83, 0x00, 0x60, 0x00,
        // CR3=P4, enable PAE, EFER.LME, paging
        0xb8, 0x00, 0x10, 0x00, 0x00, 0x0f, 0x22, 0xd8,
        0x0f, 0x20, 0xe0, 0x83, 0xc8, 0x20, 0x0f, 0x22, 0xe0,
        0xb9, 0x80, 0x00, 0x00, 0xc0, 0x0f, 0x32, 0x0d, 0x00, 0x01, 0x00, 0x00, 0x0f, 0x30,
        0x0f, 0x20, 0xc0, 0x0d, 0x00, 0x00, 0x00, 0x80, 0x0f, 0x22, 0xc0,
        // lgdt [gdt_ptr]; ljmp 0x08:long_mode (addresses patched below)
        0x0f, 0x01, 0x15, 0xc8, 0x00, 0x00, 0x00,  // lgdt (addr at offset 0x89)
        0xea, 0x94, 0x00, 0x00, 0x00, 0x08, 0x00,  // ljmp (addr at offset 0x8E)
        // 64-bit code (long_mode at offset 0x94)
        0x66, 0x31, 0xc0, 0x8e, 0xd8, 0x8e, 0xc0, 0x8e, 0xd0,
        0x48, 0xbc, 0x00, 0x00, 0x20, 0x00, 0x00, 0x00, 0x00, 0x00, // mov rsp, 0x200000
        // call main (0xE8 + rel32 placeholder at offset 0xA7)
        0xe8, 0x00, 0x00, 0x00, 0x00,
        // hlt loop
        0xf4, 0xeb, 0xfd, 0x90,
        // GDT (at offset 0xB0, 8-byte aligned)
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // null
        0xff, 0xff, 0x00, 0x00, 0x00, 0x9a, 0xaf, 0x00, // 64-bit code
        0xff, 0xff, 0x00, 0x00, 0x00, 0x92, 0xaf, 0x00, // 64-bit data
        // GDT pointer (at offset 0xC8)
        0x17, 0x00,             // limit
        0xb0, 0x00, 0x00, 0x00, // base (patched below)
    ]
}

/// Generate a bare-metal Multiboot ELF32 binary.
///
/// Layout: [ELF32 header + phdr] [bootstrap] [runtime] [user code]
/// Load address: 0x100000
/// Entry point: 0x10000C (_start in bootstrap, after Multiboot header)
pub fn generate_baremetal_elf(encoded: &EncodedProgram, entry: &str) -> Vec<u8> {
    use crate::backend::runtime;

    let bootstrap = bootstrap_blob();
    let bootstrap_size = bootstrap.len();

    let runtime_blob = runtime::runtime_code();
    let runtime_syms = runtime::runtime_symbols();
    let runtime_size = runtime_blob.len();

    // Combine: [bootstrap | runtime | user_code]
    let mut code = bootstrap;
    code.extend_from_slice(&runtime_blob);
    code.extend_from_slice(&encoded.code);

    // Build symbol table
    let mut symbols: HashMap<String, usize> = HashMap::new();

    // Runtime symbols (offset from start of code = bootstrap_size + runtime_offset)
    for (name, offset) in &runtime_syms {
        symbols.insert(name.clone(), bootstrap_size + offset);
    }

    // User symbols (offset from start of code = bootstrap_size + runtime_size + user_offset)
    for (name, &offset) in &encoded.symbols {
        symbols.insert(name.clone(), bootstrap_size + runtime_size + offset);
    }

    // Patch the bootstrap's `call main` placeholder
    if let Some(&main_offset) = symbols.get(entry) {
        let call_addr = BOOTSTRAP_CALL_PATCH_OFFSET;
        let rel = (main_offset as i64) - (call_addr as i64 + 4);
        code[call_addr + 1..call_addr + 5].copy_from_slice(&(rel as i32).to_le_bytes());
    }

    // Resolve user code relocations
    for reloc in &encoded.relocations {
        if let Some(&target_pos) = symbols.get(&reloc.target) {
            let patch_offset = bootstrap_size + runtime_size + reloc.offset;
            let rel = (target_pos as i64) - (patch_offset as i64 + 4);
            code[patch_offset..patch_offset + 4].copy_from_slice(&(rel as i32).to_le_bytes());
        }
    }

    // The ELF headers (84 bytes) are loaded at BAREMETAL_VADDR because p_offset=0.
    // All code addresses must account for this header offset.
    let header_size = ELF32_EHDR_SIZE as usize + ELF32_PHDR_SIZE as usize;
    let code_base = BAREMETAL_VADDR + header_size as u32;

    // Patch bootstrap addresses that reference absolute memory locations.
    // These were assembled with base 0, need to be offset by code_base.
    let gdt_ptr_addr = code_base + BOOTSTRAP_GDT_OFFSET as u32; // GDT pointer's base field
    code[BOOTSTRAP_GDT_PTR_BASE_OFFSET..BOOTSTRAP_GDT_PTR_BASE_OFFSET + 4]
        .copy_from_slice(&gdt_ptr_addr.to_le_bytes());
    let gdt_ptr_loc = code_base + 0xC8; // gdt_ptr struct location
    code[BOOTSTRAP_LGDT_ADDR_OFFSET..BOOTSTRAP_LGDT_ADDR_OFFSET + 4]
        .copy_from_slice(&gdt_ptr_loc.to_le_bytes());
    let long_mode_addr = code_base + BOOTSTRAP_LONG_MODE_OFFSET as u32;
    code[BOOTSTRAP_LJMP_ADDR_OFFSET..BOOTSTRAP_LJMP_ADDR_OFFSET + 4]
        .copy_from_slice(&long_mode_addr.to_le_bytes());

    // Build ELF32
    let total_file_size = header_size + code.len();
    let total_mem_size = total_file_size + 0x10000; // extra for BSS (page tables + stack)
    let entry_point = code_base + 0x0C; // _start after Multiboot header

    let mut output = Vec::new();

    // ELF32 header (52 bytes)
    output.extend_from_slice(&ELF_MAGIC);           // e_ident[0..4]
    output.push(ELFCLASS32);                          // e_ident[4] = class (32-bit)
    output.push(ELFDATA2LSB);                         // e_ident[5] = data (little-endian)
    output.push(EV_CURRENT);                          // e_ident[6] = version
    output.push(ELFOSABI_NONE);                       // e_ident[7] = OS/ABI
    output.extend_from_slice(&[0; 8]);                // e_ident[8..16] = padding
    output.extend_from_slice(&ET_EXEC.to_le_bytes()); // e_type
    output.extend_from_slice(&EM_386.to_le_bytes());  // e_machine (i386 for Multiboot compat)
    output.extend_from_slice(&1u32.to_le_bytes());    // e_version
    output.extend_from_slice(&entry_point.to_le_bytes()); // e_entry
    output.extend_from_slice(&(ELF32_EHDR_SIZE as u32).to_le_bytes()); // e_phoff
    output.extend_from_slice(&0u32.to_le_bytes());    // e_shoff
    output.extend_from_slice(&0u32.to_le_bytes());    // e_flags
    output.extend_from_slice(&ELF32_EHDR_SIZE.to_le_bytes()); // e_ehsize
    output.extend_from_slice(&ELF32_PHDR_SIZE.to_le_bytes()); // e_phentsize
    output.extend_from_slice(&1u16.to_le_bytes());    // e_phnum
    output.extend_from_slice(&0u16.to_le_bytes());    // e_shentsize
    output.extend_from_slice(&0u16.to_le_bytes());    // e_shnum
    output.extend_from_slice(&0u16.to_le_bytes());    // e_shstrndx

    // Program header (32 bytes for ELF32)
    output.extend_from_slice(&PT_LOAD.to_le_bytes()); // p_type
    output.extend_from_slice(&0u32.to_le_bytes());    // p_offset
    output.extend_from_slice(&BAREMETAL_VADDR.to_le_bytes()); // p_vaddr
    output.extend_from_slice(&BAREMETAL_VADDR.to_le_bytes()); // p_paddr
    output.extend_from_slice(&(total_file_size as u32).to_le_bytes()); // p_filesz
    output.extend_from_slice(&(total_mem_size as u32).to_le_bytes());  // p_memsz
    output.extend_from_slice(&(PF_R | PF_W | PF_X).to_le_bytes());    // p_flags
    output.extend_from_slice(&0x1000u32.to_le_bytes()); // p_align

    // Code (bootstrap + runtime + user)
    output.extend_from_slice(&code);

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
