//! DTAL Text Parser
//!
//! Parses the text format emitted by the DTAL emitter back into
//! `DtalProgram`, enabling standalone verification from DTAL files.

use crate::backend::dtal::constraints::{Constraint, IndexExpr};
use crate::backend::dtal::instr::{
    BinaryOp, CmpOp, DtalBlock, DtalFunction, DtalInstr, DtalProgram, TypeState,
};
use crate::backend::dtal::regs::{PhysicalReg, Reg, VirtualReg};
use crate::backend::dtal::types::DtalType;
use crate::common::ownership::OwnershipMode;
use std::sync::Arc;

/// Parse error for DTAL text
#[derive(Debug, Clone)]
pub struct DtalParseError {
    pub line: usize,
    pub msg: String,
}

impl std::fmt::Display for DtalParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}: {}", self.line, self.msg)
    }
}

impl std::error::Error for DtalParseError {}

/// Parse DTAL text into a program
pub fn parse_dtal(input: &str) -> Result<DtalProgram, Vec<DtalParseError>> {
    let mut parser = DtalParser::new(input);
    parser.parse_program()
}

struct DtalParser<'a> {
    lines: Vec<&'a str>,
    pos: usize,
}

impl<'a> DtalParser<'a> {
    fn new(input: &'a str) -> Self {
        Self {
            lines: input.lines().collect(),
            pos: 0,
        }
    }

    fn current_line(&self) -> Option<&'a str> {
        self.lines.get(self.pos).copied()
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn line_num(&self) -> usize {
        self.pos + 1
    }

    fn err(&self, msg: impl Into<String>) -> DtalParseError {
        DtalParseError {
            line: self.line_num(),
            msg: msg.into(),
        }
    }

    fn skip_blank_and_comments(&mut self) {
        while let Some(line) = self.current_line() {
            let trimmed = line.trim();
            if trimmed.is_empty()
                || (trimmed.starts_with(';')
                    && !trimmed.starts_with("; type ")
                    && !trimmed.starts_with("; assume ")
                    && !trimmed.starts_with("; assert "))
            {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn parse_program(&mut self) -> Result<DtalProgram, Vec<DtalParseError>> {
        let mut functions = Vec::new();
        let mut errors = Vec::new();

        // Skip header comments
        while let Some(line) = self.current_line() {
            let trimmed = line.trim();
            if trimmed.starts_with(';') || trimmed.is_empty() {
                self.advance();
            } else {
                break;
            }
        }

        while self.pos < self.lines.len() {
            self.skip_blank_and_comments();
            if self.pos >= self.lines.len() {
                break;
            }

            match self.parse_function() {
                Ok(func) => functions.push(func),
                Err(e) => {
                    errors.push(e);
                    // Skip to next .function directive
                    self.advance();
                    while let Some(line) = self.current_line() {
                        if line.trim().starts_with(".function ") {
                            break;
                        }
                        self.advance();
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(DtalProgram { functions })
        } else {
            Err(errors)
        }
    }

    fn parse_function(&mut self) -> Result<DtalFunction, DtalParseError> {
        // .function <name>
        let line = self
            .current_line()
            .ok_or_else(|| self.err("expected .function"))?;
        let trimmed = line.trim();
        if !trimmed.starts_with(".function ") {
            return Err(self.err(format!("expected .function, got '{}'", trimmed)));
        }
        let name = trimmed[".function ".len()..].trim().to_string();
        self.advance();

        // Parse directives
        let mut params = Vec::new();
        let mut return_type = DtalType::Unit;
        let mut precondition = None;
        let mut postcondition = None;

        while let Some(line) = self.current_line() {
            let trimmed = line.trim();
            if trimmed.starts_with(".params {") {
                params = self.parse_params(trimmed)?;
                self.advance();
            } else if let Some(rest) = trimmed.strip_prefix(".returns ") {
                return_type = parse_type_str(rest.trim())?;
                self.advance();
            } else if let Some(rest) = trimmed.strip_prefix(".precondition ") {
                precondition = Some(parse_constraint_str(rest.trim())?);
                self.advance();
            } else if let Some(rest) = trimmed.strip_prefix(".postcondition ") {
                postcondition = Some(parse_constraint_str(rest.trim())?);
                self.advance();
            } else {
                break;
            }
        }

        // Skip blank lines
        while let Some(line) = self.current_line() {
            if line.trim().is_empty() {
                self.advance();
            } else {
                break;
            }
        }

        // Skip entry point label (function_name:)
        if let Some(line) = self.current_line() {
            let trimmed = line.trim();
            if trimmed.ends_with(':') && !trimmed.starts_with('.') {
                self.advance();
            }
        }

        // Parse blocks
        let mut blocks = Vec::new();
        while let Some(line) = self.current_line() {
            let trimmed = line.trim();
            if trimmed.starts_with('.') && trimmed.ends_with(':') {
                blocks.push(self.parse_block()?);
            } else if trimmed.starts_with(".function ") || trimmed.is_empty() {
                break;
            } else {
                self.advance();
            }
        }

        Ok(DtalFunction {
            name,
            parameter_ownerships: vec![OwnershipMode::Plain; params.len()],
            params,
            return_type,
            precondition,
            postcondition,
            blocks,
        })
    }

    fn parse_params(&self, line: &str) -> Result<Vec<(Reg, DtalType)>, DtalParseError> {
        // .params {v0: int, v1: bool}
        let content = line.trim_start_matches(".params ").trim();
        let inner = content
            .strip_prefix('{')
            .and_then(|s| s.strip_suffix('}'))
            .unwrap_or(content);
        if inner.is_empty() {
            return Ok(Vec::new());
        }

        let mut params = Vec::new();
        for part in split_top_level(inner, ',') {
            let part = part.trim();
            if part.is_empty() {
                continue;
            }
            // Use top-level ':' to avoid matching inside nested types like {v: int | ...}
            let colon_pos = find_top_level_char(part, ':')
                .ok_or_else(|| self.err(format!("expected ':' in param '{}'", part)))?;
            let reg_str = part[..colon_pos].trim();
            let ty_str = part[colon_pos + 1..].trim();
            let reg = parse_reg(reg_str)
                .ok_or_else(|| self.err(format!("invalid register '{}'", reg_str)))?;
            let ty = parse_type_str(ty_str)?;
            params.push((reg, ty));
        }
        Ok(params)
    }

    fn parse_block(&mut self) -> Result<DtalBlock, DtalParseError> {
        // label:
        let line = self
            .current_line()
            .ok_or_else(|| self.err("expected block label"))?;
        let label = line.trim().trim_end_matches(':').to_string();
        self.advance();

        let mut entry_state = TypeState::new();

        // Parse entry state directives and skip legacy comments
        while let Some(line) = self.current_line() {
            let trimmed = line.trim();

            // Parse .entry {reg: type, ...} directive
            if let Some(content) = trimmed.strip_prefix(".entry ") {
                if let Some(inner) = content.strip_prefix('{').and_then(|s| s.strip_suffix('}')) {
                    for pair in split_top_level(inner, ',') {
                        let pair = pair.trim();
                        if pair.is_empty() {
                            continue;
                        }
                        if let Some(colon) = find_top_level_char(pair, ':') {
                            let reg_str = pair[..colon].trim();
                            let ty_str = pair[colon + 1..].trim();
                            let reg = parse_reg(reg_str).ok_or_else(|| {
                                self.err(format!("bad register in .entry: '{}'", reg_str))
                            })?;
                            let ty = parse_type_str(ty_str)?;
                            entry_state.register_types.insert(reg, ty);
                        }
                    }
                }
                self.advance();
                continue;
            }
            if let Some(content) = trimmed.strip_prefix(".owned ") {
                if let Some(inner) = content.strip_prefix('{').and_then(|s| s.strip_suffix('}')) {
                    for reg_str in split_top_level(inner, ',') {
                        let reg_str = reg_str.trim();
                        if reg_str.is_empty() {
                            continue;
                        }
                        let reg = parse_reg(reg_str).ok_or_else(|| {
                            self.err(format!("bad register in .owned: '{}'", reg_str))
                        })?;
                        entry_state.owned_registers.insert(reg);
                    }
                } else {
                    return Err(self.err("expected braces in .owned directive"));
                }
                self.advance();
                continue;
            }

            // Parse .assume constraint directive
            if let Some(constraint_str) = trimmed.strip_prefix(".assume ") {
                let constraint = parse_constraint_str(constraint_str)?;
                entry_state.constraints.push(constraint);
                self.advance();
                continue;
            }

            // Skip legacy entry state comments
            if trimmed == "; Entry state:" {
                self.advance();
                while let Some(line) = self.current_line() {
                    let t = line.trim();
                    if t.starts_with(";   ") {
                        self.advance();
                    } else {
                        break;
                    }
                }
                continue;
            }

            break;
        }

        // Parse instructions
        let mut instructions = Vec::new();
        while let Some(line) = self.current_line() {
            let trimmed = line.trim();

            // Stop at next block, next function, or blank line between functions
            if (trimmed.ends_with(':') && !trimmed.starts_with(';'))
                || trimmed.starts_with(".function ")
            {
                break;
            }

            // Blank line — could be end of function
            if trimmed.is_empty() {
                break;
            }

            if let Some(instr) = self.parse_instruction(trimmed)? {
                instructions.push(instr);
            }
            self.advance();
        }

        Ok(DtalBlock {
            label,
            entry_state,
            instructions,
        })
    }

    fn parse_instruction(&self, line: &str) -> Result<Option<DtalInstr>, DtalParseError> {
        let trimmed = line.trim();

        // Annotation instructions as directives
        if trimmed.starts_with(".type ") {
            return self.parse_type_annotation(trimmed);
        }
        if trimmed.starts_with(".assert ") {
            return self.parse_constraint_assert(trimmed);
        }
        // .assume in instruction stream: skip (block-level .assume is handled
        // in parse_block; instruction-level assumes are no longer emitted)
        if trimmed.starts_with(".assume ") {
            return Ok(None);
        }
        // Legacy comment-style annotation instructions (backward compatibility)
        if trimmed.starts_with("; type ") {
            return self.parse_type_annotation_legacy(trimmed);
        }
        if trimmed.starts_with("; assume ") {
            return Ok(None); // Legacy assume — skip
        }
        if trimmed.starts_with("; assert ") {
            return self.parse_constraint_assert_legacy(trimmed);
        }
        // Regular comments
        if trimmed.starts_with(';') {
            return Ok(None);
        }

        // Split instruction from type annotation comment
        let (instr_part, ty_comment) = split_instruction_comment(trimmed);
        let tokens: Vec<&str> = instr_part.split_whitespace().collect();

        if tokens.is_empty() {
            return Ok(None);
        }

        match tokens[0] {
            "mov" => self.parse_mov(&tokens, ty_comment),
            "alias_borrow" => self.parse_alias_borrow(&tokens, ty_comment),
            "move_owned" => self.parse_move_owned(&tokens, ty_comment),
            "load" => self.parse_load(&tokens, ty_comment),
            "store" => self.parse_store(&tokens),
            "add" | "sub" | "mul" | "div" | "and" | "or" => self.parse_binop(&tokens, ty_comment),
            "addi" => self.parse_addi(&tokens, ty_comment),
            "cmp" => self.parse_cmp(&tokens),
            "not" => self.parse_not(&tokens, ty_comment),
            "neg" => self.parse_neg(&tokens, ty_comment),
            "shli" => self.parse_shli(&tokens, ty_comment),
            "shri" => self.parse_shri(&tokens, ty_comment),
            "jmp" => self.parse_jmp(&tokens),
            "call" | "call_owned" | "call_consume" => self.parse_call(&tokens, ty_comment),
            "ret" => Ok(Some(DtalInstr::Ret)),
            "push" => self.parse_push(&tokens, ty_comment),
            "pop" => self.parse_pop(&tokens, ty_comment),
            "alloca" => self.parse_alloca(&tokens, ty_comment),
            "drop_owned" => self.parse_drop_owned(&tokens, ty_comment),
            _ if tokens[0].starts_with("set") => self.parse_setcc(&tokens),
            _ if tokens[0].starts_with('b') && tokens.len() >= 2 => self.parse_branch(&tokens),
            _ => Ok(None), // Unknown instruction, skip
        }
    }

    fn parse_type_annotation(&self, line: &str) -> Result<Option<DtalInstr>, DtalParseError> {
        // .type v0: int
        let rest = line.trim_start_matches(".type ").trim();
        let colon = rest
            .find(':')
            .ok_or_else(|| self.err("expected ':' in type annotation"))?;
        let reg_str = rest[..colon].trim();
        let ty_str = rest[colon + 1..].trim();
        let reg = parse_reg(reg_str)
            .ok_or_else(|| self.err(format!("invalid register '{}'", reg_str)))?;
        let ty = parse_type_str(ty_str)?;
        Ok(Some(DtalInstr::TypeAnnotation { reg, ty }))
    }

    fn parse_constraint_assert(&self, line: &str) -> Result<Option<DtalInstr>, DtalParseError> {
        // .assert <constraint>
        let rest = line.trim_start_matches(".assert ").trim();
        // Strip any trailing legacy " ; msg" comment
        let constraint_str = if let Some(pos) = rest.find(" ; ") {
            &rest[..pos]
        } else {
            rest
        };
        let constraint = parse_constraint_str(constraint_str)?;
        Ok(Some(DtalInstr::ConstraintAssert { constraint }))
    }

    // Legacy comment-style annotation parsers (backward compatibility)

    fn parse_type_annotation_legacy(
        &self,
        line: &str,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        let rest = line.trim_start_matches("; type ").trim();
        let colon = rest
            .find(':')
            .ok_or_else(|| self.err("expected ':' in type annotation"))?;
        let reg_str = rest[..colon].trim();
        let ty_str = rest[colon + 1..].trim();
        let reg = parse_reg(reg_str)
            .ok_or_else(|| self.err(format!("invalid register '{}'", reg_str)))?;
        let ty = parse_type_str(ty_str)?;
        Ok(Some(DtalInstr::TypeAnnotation { reg, ty }))
    }

    fn parse_constraint_assert_legacy(
        &self,
        line: &str,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        let rest = line.trim_start_matches("; assert ").trim();
        let constraint_str = if let Some(pos) = rest.find(" ; ") {
            &rest[..pos]
        } else {
            rest
        };
        let constraint = parse_constraint_str(constraint_str)?;
        Ok(Some(DtalInstr::ConstraintAssert { constraint }))
    }

    fn parse_mov(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        // mov v0, 42    ; int
        // mov v0, v1    ; int
        if tokens.len() < 3 {
            return Err(self.err("mov requires at least 2 operands"));
        }
        let dst_str = tokens[1].trim_end_matches(',');
        let src_str = tokens[2].trim_end_matches(',');
        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);

        if let Some(reg) = parse_reg(src_str) {
            Ok(Some(DtalInstr::MovReg { dst, src: reg, ty }))
        } else if let Ok(imm) = src_str.parse::<i128>() {
            Ok(Some(DtalInstr::MovImm { dst, imm, ty }))
        } else {
            Err(self.err(format!("invalid mov source '{}'", src_str)))
        }
    }

    fn parse_load(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        // load v0, [v1 + v2]    ; int
        let full = tokens[1..].join(" ");
        let dst_end = full
            .find(',')
            .ok_or_else(|| self.err("expected ',' in load"))?;
        let dst_str = full[..dst_end].trim();
        let rest = full[dst_end + 1..].trim();

        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;

        // Parse [base + offset]
        let inner = rest.trim_start_matches('[').trim_end_matches(']').trim();
        let plus_pos = inner
            .find('+')
            .ok_or_else(|| self.err("expected '+' in load address"))?;
        let base_str = inner[..plus_pos].trim();
        let offset_str = inner[plus_pos + 1..].trim();

        let base =
            parse_reg(base_str).ok_or_else(|| self.err(format!("invalid base '{}'", base_str)))?;
        let offset = parse_reg(offset_str)
            .ok_or_else(|| self.err(format!("invalid offset '{}'", offset_str)))?;

        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);

        Ok(Some(DtalInstr::Load {
            dst,
            base,
            offset,
            ty,
        }))
    }

    fn parse_move_owned(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        if tokens.len() < 3 {
            return Err(self.err("move_owned requires 2 operands"));
        }
        let dst_str = tokens[1].trim_end_matches(',');
        let src_str = tokens[2].trim_end_matches(',');
        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        let src =
            parse_reg(src_str).ok_or_else(|| self.err(format!("invalid src '{}'", src_str)))?;
        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);
        Ok(Some(DtalInstr::MoveOwned { dst, src, ty }))
    }

    fn parse_alias_borrow(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        if tokens.len() < 3 {
            return Err(self.err("alias_borrow requires 2 operands"));
        }
        let dst_str = tokens[1].trim_end_matches(',');
        let src_str = tokens[2];
        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        let src =
            parse_reg(src_str).ok_or_else(|| self.err(format!("invalid src '{}'", src_str)))?;
        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);
        Ok(Some(DtalInstr::AliasBorrow { dst, src, ty }))
    }

    fn parse_store(&self, tokens: &[&str]) -> Result<Option<DtalInstr>, DtalParseError> {
        // store [v0 + v1], v2
        let full = tokens[1..].join(" ");
        let bracket_end = full
            .find(']')
            .ok_or_else(|| self.err("expected ']' in store"))?;
        let addr = full[..bracket_end].trim_start_matches('[').trim();
        let src_str = full[bracket_end + 1..].trim_start_matches(',').trim();

        let plus_pos = addr
            .find('+')
            .ok_or_else(|| self.err("expected '+' in store address"))?;
        let base_str = addr[..plus_pos].trim();
        let offset_str = addr[plus_pos + 1..].trim();

        let base =
            parse_reg(base_str).ok_or_else(|| self.err(format!("invalid base '{}'", base_str)))?;
        let offset = parse_reg(offset_str)
            .ok_or_else(|| self.err(format!("invalid offset '{}'", offset_str)))?;
        let src =
            parse_reg(src_str).ok_or_else(|| self.err(format!("invalid src '{}'", src_str)))?;

        Ok(Some(DtalInstr::Store { base, offset, src }))
    }

    fn parse_binop(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        // add v0, v1, v2    ; int
        if tokens.len() < 4 {
            return Err(self.err("binop requires 3 operands"));
        }
        let op = match tokens[0] {
            "add" => BinaryOp::Add,
            "sub" => BinaryOp::Sub,
            "mul" => BinaryOp::Mul,
            "div" => BinaryOp::Div,
            "mod" => BinaryOp::Mod,
            "bitand" => BinaryOp::BitAnd,
            "bitor" => BinaryOp::BitOr,
            "bitxor" => BinaryOp::BitXor,
            "shl" => BinaryOp::Shl,
            "shr" => BinaryOp::Shr,
            "and" => BinaryOp::And,
            "or" => BinaryOp::Or,
            _ => return Err(self.err(format!("unknown binop '{}'", tokens[0]))),
        };
        let dst_str = tokens[1].trim_end_matches(',');
        let lhs_str = tokens[2].trim_end_matches(',');
        let rhs_str = tokens[3].trim_end_matches(',');

        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        let lhs =
            parse_reg(lhs_str).ok_or_else(|| self.err(format!("invalid lhs '{}'", lhs_str)))?;
        let rhs =
            parse_reg(rhs_str).ok_or_else(|| self.err(format!("invalid rhs '{}'", rhs_str)))?;

        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);

        Ok(Some(DtalInstr::BinOp {
            op,
            dst,
            lhs,
            rhs,
            ty,
        }))
    }

    fn parse_addi(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        // addi v0, v1, 42    ; int
        if tokens.len() < 4 {
            return Err(self.err("addi requires 3 operands"));
        }
        let dst_str = tokens[1].trim_end_matches(',');
        let src_str = tokens[2].trim_end_matches(',');
        let imm_str = tokens[3].trim_end_matches(',');

        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        let src =
            parse_reg(src_str).ok_or_else(|| self.err(format!("invalid src '{}'", src_str)))?;
        let imm: i128 = imm_str
            .parse()
            .map_err(|_| self.err(format!("invalid imm '{}'", imm_str)))?;

        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);

        Ok(Some(DtalInstr::AddImm { dst, src, imm, ty }))
    }

    fn parse_cmp(&self, tokens: &[&str]) -> Result<Option<DtalInstr>, DtalParseError> {
        // cmp v0, v1  OR  cmp v0, 42
        if tokens.len() < 3 {
            return Err(self.err("cmp requires 2 operands"));
        }
        let lhs_str = tokens[1].trim_end_matches(',');
        let rhs_str = tokens[2].trim_end_matches(',');

        let lhs =
            parse_reg(lhs_str).ok_or_else(|| self.err(format!("invalid lhs '{}'", lhs_str)))?;

        if let Some(rhs_reg) = parse_reg(rhs_str) {
            Ok(Some(DtalInstr::Cmp { lhs, rhs: rhs_reg }))
        } else if let Ok(imm) = rhs_str.parse::<i128>() {
            Ok(Some(DtalInstr::CmpImm { lhs, imm }))
        } else {
            Err(self.err(format!("invalid cmp rhs '{}'", rhs_str)))
        }
    }

    fn parse_setcc(&self, tokens: &[&str]) -> Result<Option<DtalInstr>, DtalParseError> {
        // seteq v0
        let cond_str = &tokens[0][3..]; // skip "set"
        let cond = parse_cmpop(cond_str)
            .ok_or_else(|| self.err(format!("invalid setcc condition '{}'", cond_str)))?;
        if tokens.len() < 2 {
            return Err(self.err("setcc requires 1 operand"));
        }
        let dst_str = tokens[1].trim_end_matches(',');
        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        Ok(Some(DtalInstr::SetCC { dst, cond }))
    }

    fn parse_not(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        // not v0, v1    ; bool
        if tokens.len() < 3 {
            return Err(self.err("not requires 2 operands"));
        }
        let dst_str = tokens[1].trim_end_matches(',');
        let src_str = tokens[2].trim_end_matches(',');

        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        let src =
            parse_reg(src_str).ok_or_else(|| self.err(format!("invalid src '{}'", src_str)))?;

        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Bool);

        Ok(Some(DtalInstr::Not { dst, src, ty }))
    }

    fn parse_neg(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        // neg v0, v1    : int
        if tokens.len() < 3 {
            return Err(self.err("neg requires 2 operands"));
        }
        let dst_str = tokens[1].trim_end_matches(',');
        let src_str = tokens[2].trim_end_matches(',');

        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        let src =
            parse_reg(src_str).ok_or_else(|| self.err(format!("invalid src '{}'", src_str)))?;

        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);

        Ok(Some(DtalInstr::Neg { dst, src, ty }))
    }

    fn parse_shli(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        if tokens.len() < 4 {
            return Err(self.err("shli requires 3 operands"));
        }
        let dst_str = tokens[1].trim_end_matches(',');
        let src_str = tokens[2].trim_end_matches(',');
        let imm_str = tokens[3].trim_end_matches(',');
        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        let src =
            parse_reg(src_str).ok_or_else(|| self.err(format!("invalid src '{}'", src_str)))?;
        let imm: u8 = imm_str
            .parse()
            .map_err(|_| self.err(format!("invalid imm '{}'", imm_str)))?;
        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);
        Ok(Some(DtalInstr::ShlImm { dst, src, imm, ty }))
    }

    fn parse_shri(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        if tokens.len() < 4 {
            return Err(self.err("shri requires 3 operands"));
        }
        let dst_str = tokens[1].trim_end_matches(',');
        let src_str = tokens[2].trim_end_matches(',');
        let imm_str = tokens[3].trim_end_matches(',');
        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        let src =
            parse_reg(src_str).ok_or_else(|| self.err(format!("invalid src '{}'", src_str)))?;
        let imm: u8 = imm_str
            .parse()
            .map_err(|_| self.err(format!("invalid imm '{}'", imm_str)))?;
        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);
        Ok(Some(DtalInstr::ShrImm { dst, src, imm, ty }))
    }

    fn parse_jmp(&self, tokens: &[&str]) -> Result<Option<DtalInstr>, DtalParseError> {
        if tokens.len() < 2 {
            return Err(self.err("jmp requires target"));
        }
        Ok(Some(DtalInstr::Jmp {
            target: tokens[1].to_string(),
        }))
    }

    fn parse_branch(&self, tokens: &[&str]) -> Result<Option<DtalInstr>, DtalParseError> {
        // beq .label  OR  bne .label
        let cond_str = &tokens[0][1..]; // skip 'b'
        let cond = parse_cmpop(cond_str)
            .ok_or_else(|| self.err(format!("invalid branch condition '{}'", cond_str)))?;
        if tokens.len() < 2 {
            return Err(self.err("branch requires target"));
        }
        Ok(Some(DtalInstr::Branch {
            cond,
            target: tokens[1].to_string(),
        }))
    }

    fn parse_call(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        // call foo    : int   (new syntax)
        // call foo    ; -> int   (legacy)
        if tokens.len() < 2 {
            return Err(self.err("call requires target"));
        }
        let return_ty = ty_comment
            .and_then(|s| {
                let s = s.trim();
                // Legacy: strip "-> " prefix if present
                if let Some(rest) = s.strip_prefix("-> ") {
                    parse_type_str(rest.trim()).ok()
                } else {
                    parse_type_str(s).ok()
                }
            })
            .unwrap_or(DtalType::Unit);

        Ok(Some(DtalInstr::Call {
            target: tokens[1].to_string(),
            arg_ownerships: tokens
                .get(2)
                .and_then(|token| token.strip_prefix('[').and_then(|rest| rest.strip_suffix(']')))
                .map(|effects| {
                    if effects.is_empty() {
                        Ok(Vec::new())
                    } else {
                        effects
                            .split(',')
                            .map(|effect| match effect {
                                "plain" => Ok(OwnershipMode::Plain),
                                "consume" => Ok(OwnershipMode::Consume),
                                "fresh" => Ok(OwnershipMode::FreshOwned),
                                other => Err(self.err(format!(
                                    "invalid call ownership effect '{}'",
                                    other
                                ))),
                            })
                            .collect()
                    }
                })
                .transpose()?
                .unwrap_or_default(),
            return_ty,
            ownership: if tokens[0] == "call_owned" {
                OwnershipMode::FreshOwned
            } else if tokens[0] == "call_consume" {
                OwnershipMode::Consume
            } else {
                OwnershipMode::Plain
            },
        }))
    }

    fn parse_push(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        if tokens.len() < 2 {
            return Err(self.err("push requires 1 operand"));
        }
        let src_str = tokens[1].trim_end_matches(',');
        let src =
            parse_reg(src_str).ok_or_else(|| self.err(format!("invalid src '{}'", src_str)))?;
        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);
        Ok(Some(DtalInstr::Push { src, ty }))
    }

    fn parse_pop(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        if tokens.len() < 2 {
            return Err(self.err("pop requires 1 operand"));
        }
        let dst_str = tokens[1].trim_end_matches(',');
        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);
        Ok(Some(DtalInstr::Pop { dst, ty }))
    }

    fn parse_alloca(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        // alloca v0, 80    ; [int; 10]
        if tokens.len() < 3 {
            return Err(self.err("alloca requires 2 operands"));
        }
        let dst_str = tokens[1].trim_end_matches(',');
        let size_str = tokens[2].trim_end_matches(',');

        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        let size: u32 = size_str
            .parse()
            .map_err(|_| self.err(format!("invalid size '{}'", size_str)))?;

        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);

        Ok(Some(DtalInstr::Alloca { dst, size, ty }))
    }

    fn parse_drop_owned(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        if tokens.len() < 2 {
            return Err(self.err("drop_owned requires 1 operand"));
        }
        let src_str = tokens[1].trim_end_matches(',');
        let src =
            parse_reg(src_str).ok_or_else(|| self.err(format!("invalid src '{}'", src_str)))?;
        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);
        Ok(Some(DtalInstr::DropOwned { src, ty }))
    }
}

// === Helper functions ===

/// Parse a register name like "v0", "r0", "sp", "fp", "lr"
fn parse_reg(s: &str) -> Option<Reg> {
    let s = s.trim();
    if let Some(rest) = s.strip_prefix('v') {
        let n: u32 = rest.parse().ok()?;
        Some(Reg::Virtual(VirtualReg(n)))
    } else {
        let preg = match s {
            "r0" => PhysicalReg::R0,
            "r1" => PhysicalReg::R1,
            "r2" => PhysicalReg::R2,
            "r3" => PhysicalReg::R3,
            "r4" => PhysicalReg::R4,
            "r5" => PhysicalReg::R5,
            "r6" => PhysicalReg::R6,
            "r7" => PhysicalReg::R7,
            "r8" => PhysicalReg::R8,
            "r9" => PhysicalReg::R9,
            "r10" => PhysicalReg::R10,
            "r11" => PhysicalReg::R11,
            "r12" => PhysicalReg::R12,
            "r13" => PhysicalReg::R13,
            "r14" => PhysicalReg::R14,
            "r15" => PhysicalReg::R15,
            "sp" => PhysicalReg::SP,
            "fp" => PhysicalReg::FP,
            "lr" => PhysicalReg::LR,
            _ => return None,
        };
        Some(Reg::Physical(preg))
    }
}

/// Parse a comparison operation name
fn parse_cmpop(s: &str) -> Option<CmpOp> {
    match s {
        "eq" => Some(CmpOp::Eq),
        "ne" => Some(CmpOp::Ne),
        "lt" => Some(CmpOp::Lt),
        "le" => Some(CmpOp::Le),
        "gt" => Some(CmpOp::Gt),
        "ge" => Some(CmpOp::Ge),
        _ => None,
    }
}

/// Split instruction text from trailing type comment
/// e.g. "mov v0, 42    ; int" -> ("mov v0, 42", Some("int"))
fn split_instruction_comment(line: &str) -> (&str, Option<&str>) {
    // Find "    :" separator (4 spaces + colon) — type annotation syntax
    if let Some(pos) = line.find("    : ") {
        let instr = line[..pos].trim();
        let comment = line[pos + 6..].trim(); // skip "    : "
        return (
            instr,
            if comment.is_empty() {
                None
            } else {
                Some(comment)
            },
        );
    }
    // Legacy: "    ;" separator (4 spaces + semicolon) — backward compatibility
    if let Some(pos) = line.find("    ;") {
        let instr = line[..pos].trim();
        let comment = line[pos + 5..].trim(); // skip "    ;"
        return (
            instr,
            if comment.is_empty() {
                None
            } else {
                Some(comment)
            },
        );
    }
    (line.trim(), None)
}

/// Parse a type from a string
fn parse_type_str(s: &str) -> Result<DtalType, DtalParseError> {
    let s = s.trim();
    match s {
        "unit" | "()" => Ok(DtalType::Unit),
        "int" => Ok(DtalType::Int),
        "i64" => Ok(DtalType::I64),
        "u64" => Ok(DtalType::U64),
        "bool" => Ok(DtalType::Bool),
        _ if s.starts_with("int(") && s.ends_with(')') => {
            let inner = &s[4..s.len() - 1];
            let idx = parse_index_expr(inner)?;
            Ok(DtalType::SingletonInt(idx))
        }
        _ if s.starts_with('[') && s.ends_with(']') => {
            // [int; 10]
            let inner = &s[1..s.len() - 1];
            let semi = find_top_level_char(inner, ';')
                .ok_or_else(|| err_static(format!("expected ';' in array type '{}'", s)))?;
            let elem_str = inner[..semi].trim();
            let size_str = inner[semi + 1..].trim();
            let element_type = parse_type_str(elem_str)?;
            let size = parse_index_expr(size_str)?;
            Ok(DtalType::Array {
                element_type: Arc::new(element_type),
                size,
            })
        }
        _ if s.starts_with("&mut ") => {
            let inner = parse_type_str(&s[5..])?;
            Ok(DtalType::RefMut(Arc::new(inner)))
        }
        _ if s.starts_with('&') => {
            let inner = parse_type_str(&s[1..])?;
            Ok(DtalType::Ref(Arc::new(inner)))
        }
        _ if s.starts_with("master(") && s.ends_with(')') => {
            let inner = parse_type_str(&s[7..s.len() - 1])?;
            Ok(DtalType::Master(Arc::new(inner)))
        }
        _ if s.starts_with("exists ") => {
            // exists n. int(n) where constraint
            let rest = &s[7..]; // skip "exists "
            let dot = rest
                .find('.')
                .ok_or_else(|| err_static(format!("expected '.' in existential type '{}'", s)))?;
            let witness_var = rest[..dot].trim().to_string();
            let after_dot = rest[dot + 1..].trim();
            // Expect "int(<witness_var>) where <constraint>"
            let where_pos = after_dot.find(" where ").ok_or_else(|| {
                err_static(format!("expected 'where' in existential type '{}'", s))
            })?;
            let constraint_str = after_dot[where_pos + 7..].trim();
            let constraint = parse_constraint_str(constraint_str)?;
            Ok(DtalType::ExistentialInt {
                witness_var,
                constraint,
            })
        }
        _ if s.starts_with('{') && s.ends_with('}') => {
            // {x: int | constraint }
            let inner = &s[1..s.len() - 1].trim();
            let colon = inner
                .find(':')
                .ok_or_else(|| err_static(format!("expected ':' in refined type '{}'", s)))?;
            let var = inner[..colon].trim().to_string();
            let rest = inner[colon + 1..].trim();
            let pipe = rest
                .find('|')
                .ok_or_else(|| err_static(format!("expected '|' in refined type '{}'", s)))?;
            let base_str = rest[..pipe].trim();
            let constraint_str = rest[pipe + 1..].trim();
            let base = parse_type_str(base_str)?;
            let constraint = parse_constraint_str(constraint_str)?;
            Ok(DtalType::RefinedInt {
                base: Arc::new(base),
                var,
                constraint,
            })
        }
        _ => Err(err_static(format!("unknown type '{}'", s))),
    }
}

fn err_static(msg: String) -> DtalParseError {
    DtalParseError { line: 0, msg }
}

/// Parse a constraint from string
fn parse_constraint_str(s: &str) -> Result<Constraint, DtalParseError> {
    let s = s.trim();
    if s == "true" {
        return Ok(Constraint::True);
    }
    if s == "false" {
        return Ok(Constraint::False);
    }

    // Try to parse parenthesized expressions
    if s.starts_with('(') && s.ends_with(')') {
        let inner = &s[1..s.len() - 1];

        // Check for binary constraint operators: &&, ||, ==>
        if let Some(pos) = find_top_level_op(inner, "&&") {
            let left = parse_constraint_str(inner[..pos].trim())?;
            let right = parse_constraint_str(inner[pos + 2..].trim())?;
            return Ok(Constraint::And(Box::new(left), Box::new(right)));
        }
        if let Some(pos) = find_top_level_op(inner, "||") {
            let left = parse_constraint_str(inner[..pos].trim())?;
            let right = parse_constraint_str(inner[pos + 2..].trim())?;
            return Ok(Constraint::Or(Box::new(left), Box::new(right)));
        }
        if let Some(pos) = find_top_level_op(inner, "==>") {
            let left = parse_constraint_str(inner[..pos].trim())?;
            let right = parse_constraint_str(inner[pos + 3..].trim())?;
            return Ok(Constraint::Implies(Box::new(left), Box::new(right)));
        }

        // Check for quantifiers
        if inner.starts_with("forall ") || inner.starts_with("exists ") {
            return parse_quantifier(inner);
        }

        // Otherwise, parse as inner expression
        return parse_constraint_str(inner);
    }

    // Negation
    if let Some(rest) = s.strip_prefix('!') {
        let inner = parse_constraint_str(rest)?;
        return Ok(Constraint::Not(Box::new(inner)));
    }

    // Binary comparison operators (look for top-level)
    for (op_str, make) in &[
        (
            "==",
            Constraint::Eq as fn(IndexExpr, IndexExpr) -> Constraint,
        ),
        (
            "!=",
            Constraint::Ne as fn(IndexExpr, IndexExpr) -> Constraint,
        ),
        (
            "<=",
            Constraint::Le as fn(IndexExpr, IndexExpr) -> Constraint,
        ),
        (
            ">=",
            Constraint::Ge as fn(IndexExpr, IndexExpr) -> Constraint,
        ),
        (
            "<",
            Constraint::Lt as fn(IndexExpr, IndexExpr) -> Constraint,
        ),
        (
            ">",
            Constraint::Gt as fn(IndexExpr, IndexExpr) -> Constraint,
        ),
    ] {
        if let Some(pos) = find_top_level_cmp(s, op_str) {
            let left = parse_index_expr(s[..pos].trim())?;
            let right = parse_index_expr(s[pos + op_str.len()..].trim())?;
            return Ok(make(left, right));
        }
    }

    // Non-parenthesized && / ||
    if let Some(pos) = find_top_level_op(s, "&&") {
        let left = parse_constraint_str(s[..pos].trim())?;
        let right = parse_constraint_str(s[pos + 2..].trim())?;
        return Ok(Constraint::And(Box::new(left), Box::new(right)));
    }
    if let Some(pos) = find_top_level_op(s, "||") {
        let left = parse_constraint_str(s[..pos].trim())?;
        let right = parse_constraint_str(s[pos + 2..].trim())?;
        return Ok(Constraint::Or(Box::new(left), Box::new(right)));
    }

    Err(err_static(format!("cannot parse constraint '{}'", s)))
}

fn parse_quantifier(s: &str) -> Result<Constraint, DtalParseError> {
    // forall x in 0..10 { body }  OR  exists x in 0..10 { body }
    let is_forall = s.starts_with("forall ");
    let rest = if is_forall {
        &s["forall ".len()..]
    } else {
        &s["exists ".len()..]
    };

    let in_pos = rest
        .find(" in ")
        .ok_or_else(|| err_static(format!("expected 'in' in quantifier '{}'", s)))?;
    let var = rest[..in_pos].trim().to_string();
    let rest = &rest[in_pos + 4..];

    let dotdot = rest
        .find("..")
        .ok_or_else(|| err_static(format!("expected '..' in quantifier '{}'", s)))?;
    let lower_str = rest[..dotdot].trim();
    let rest = &rest[dotdot + 2..];

    let brace = rest
        .find('{')
        .ok_or_else(|| err_static(format!("expected '{{' in quantifier '{}'", s)))?;
    let upper_str = rest[..brace].trim();
    let body_str = rest[brace + 1..].trim().trim_end_matches('}').trim();

    let lower = parse_index_expr(lower_str)?;
    let upper = parse_index_expr(upper_str)?;
    let body = parse_constraint_str(body_str)?;

    if is_forall {
        Ok(Constraint::Forall {
            var,
            lower,
            upper,
            body: Box::new(body),
        })
    } else {
        Ok(Constraint::Exists {
            var,
            lower,
            upper,
            body: Box::new(body),
        })
    }
}

/// Parse an index expression
fn parse_index_expr(s: &str) -> Result<IndexExpr, DtalParseError> {
    let s = s.trim();

    // Parenthesized
    if s.starts_with('(') && s.ends_with(')') {
        let inner = &s[1..s.len() - 1];
        // Look for top-level +, -, *, /
        for (op_str, make) in &[
            (
                "+",
                IndexExpr::Add as fn(Box<IndexExpr>, Box<IndexExpr>) -> IndexExpr,
            ),
            (
                "-",
                IndexExpr::Sub as fn(Box<IndexExpr>, Box<IndexExpr>) -> IndexExpr,
            ),
            (
                "*",
                IndexExpr::Mul as fn(Box<IndexExpr>, Box<IndexExpr>) -> IndexExpr,
            ),
            (
                "/",
                IndexExpr::Div as fn(Box<IndexExpr>, Box<IndexExpr>) -> IndexExpr,
            ),
        ] {
            if let Some(pos) = find_top_level_arith(inner, op_str) {
                let left = parse_index_expr(inner[..pos].trim())?;
                let right = parse_index_expr(inner[pos + op_str.len()..].trim())?;
                return Ok(make(Box::new(left), Box::new(right)));
            }
        }
        return parse_index_expr(inner);
    }

    // Array access: name[idx]
    if let Some(bracket) = s.find('[')
        && s.ends_with(']')
    {
        let name = s[..bracket].to_string();
        let idx_str = &s[bracket + 1..s.len() - 1];
        let idx = parse_index_expr(idx_str)?;
        return Ok(IndexExpr::Select(name, Box::new(idx)));
    }

    // Integer constant
    if let Ok(n) = s.parse::<i128>() {
        return Ok(IndexExpr::Const(n));
    }

    // Negative integer
    if s.starts_with('-')
        && let Ok(n) = s[1..].parse::<i128>()
    {
        return Ok(IndexExpr::Const(-n));
    }

    // Variable
    if s.chars().all(|c| c.is_alphanumeric() || c == '_') {
        return Ok(IndexExpr::Var(s.to_string()));
    }

    Err(err_static(format!("cannot parse index expr '{}'", s)))
}

/// Find a top-level comparison operator (not inside parens)
fn find_top_level_cmp(s: &str, op: &str) -> Option<usize> {
    let bytes = s.as_bytes();
    let op_bytes = op.as_bytes();
    let mut depth = 0i32;
    let mut i = 0;

    while i + op_bytes.len() <= bytes.len() {
        match bytes[i] {
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' => depth -= 1,
            _ => {}
        }

        if depth == 0 && &bytes[i..i + op_bytes.len()] == op_bytes {
            // For < and >, make sure we're not matching <= or >= or ==>
            if op == "<" && i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                i += 1;
                continue;
            }
            if op == ">" && i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                i += 1;
                continue;
            }
            if op == ">" && i > 0 && bytes[i - 1] == b'=' {
                // part of ==>
                i += 1;
                continue;
            }
            if op == "==" && i > 0 && bytes[i - 1] == b'!' {
                // part of !=
                i += 1;
                continue;
            }
            if op == "==" && i + 2 < bytes.len() && bytes[i + 2] == b'>' {
                // part of ==>
                i += 1;
                continue;
            }
            return Some(i);
        }
        i += 1;
    }
    None
}

/// Find a top-level logical operator (&&, ||, ==>)
fn find_top_level_op(s: &str, op: &str) -> Option<usize> {
    let bytes = s.as_bytes();
    let op_bytes = op.as_bytes();
    let mut depth = 0i32;

    for i in 0..bytes.len() {
        match bytes[i] {
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' => depth -= 1,
            _ => {}
        }
        if depth == 0
            && i + op_bytes.len() <= bytes.len()
            && &bytes[i..i + op_bytes.len()] == op_bytes
        {
            return Some(i);
        }
    }
    None
}

/// Find a top-level arithmetic operator
fn find_top_level_arith(s: &str, op: &str) -> Option<usize> {
    find_top_level_op(s, op)
}

/// Find a character at the top level (not inside brackets/parens)
fn find_top_level_char(s: &str, ch: char) -> Option<usize> {
    let mut depth = 0i32;
    for (i, c) in s.char_indices() {
        match c {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            _ if c == ch && depth == 0 => return Some(i),
            _ => {}
        }
    }
    None
}

/// Split a string at top-level commas
fn split_top_level(s: &str, sep: char) -> Vec<&str> {
    let mut result = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;

    for (i, c) in s.char_indices() {
        match c {
            '(' | '[' | '{' => depth += 1,
            ')' | ']' | '}' => depth -= 1,
            _ if c == sep && depth == 0 => {
                result.push(&s[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    if start <= s.len() {
        result.push(&s[start..]);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_program() {
        let input = r#"; DTAL Program
; Generated by Veritas Compiler

.function main
.returns unit

main:
.main_bb0:
    mov v0, 42    ; int
    ret
"#;
        let program = parse_dtal(input).unwrap();
        assert_eq!(program.functions.len(), 1);
        assert_eq!(program.functions[0].name, "main");
        assert_eq!(program.functions[0].blocks.len(), 1);
        assert_eq!(program.functions[0].blocks[0].instructions.len(), 2);
    }

    #[test]
    fn test_parse_function_with_params() {
        let input = r#"; DTAL Program
; Generated by Veritas Compiler

.function add
.params {v0: int, v1: int}
.returns int

add:
.add_bb0:
    add v2, v0, v1    ; int
    mov r0, v2    ; int
    ret
"#;
        let program = parse_dtal(input).unwrap();
        assert_eq!(program.functions[0].params.len(), 2);
        assert_eq!(program.functions[0].return_type, DtalType::Int);
    }

    #[test]
    fn test_parse_type_basic() {
        assert_eq!(parse_type_str("int").unwrap(), DtalType::Int);
        assert_eq!(parse_type_str("bool").unwrap(), DtalType::Bool);
        assert_eq!(parse_type_str("unit").unwrap(), DtalType::Unit);
    }

    #[test]
    fn test_parse_type_singleton() {
        assert_eq!(
            parse_type_str("int(42)").unwrap(),
            DtalType::SingletonInt(IndexExpr::Const(42))
        );
    }

    #[test]
    fn test_parse_type_array() {
        let ty = parse_type_str("[int; 10]").unwrap();
        assert_eq!(
            ty,
            DtalType::Array {
                element_type: Arc::new(DtalType::Int),
                size: IndexExpr::Const(10),
            }
        );
    }

    #[test]
    fn test_parse_constraint_simple() {
        let c = parse_constraint_str("v0 >= 0").unwrap();
        assert_eq!(
            c,
            Constraint::Ge(IndexExpr::Var("v0".to_string()), IndexExpr::Const(0))
        );
    }

    #[test]
    fn test_parse_constraint_and() {
        let c = parse_constraint_str("(v0 >= 0 && v0 < 10)").unwrap();
        assert_eq!(
            c,
            Constraint::And(
                Box::new(Constraint::Ge(
                    IndexExpr::Var("v0".to_string()),
                    IndexExpr::Const(0)
                )),
                Box::new(Constraint::Lt(
                    IndexExpr::Var("v0".to_string()),
                    IndexExpr::Const(10)
                ))
            )
        );
    }

    #[test]
    fn test_parse_index_expr() {
        assert_eq!(parse_index_expr("42").unwrap(), IndexExpr::Const(42));
        assert_eq!(
            parse_index_expr("v0").unwrap(),
            IndexExpr::Var("v0".to_string())
        );
        assert_eq!(
            parse_index_expr("(v0 + 1)").unwrap(),
            IndexExpr::Add(
                Box::new(IndexExpr::Var("v0".to_string())),
                Box::new(IndexExpr::Const(1))
            )
        );
    }

    #[test]
    fn test_round_trip_simple() {
        use crate::backend::emit::emit_program;

        let input_program = DtalProgram {
            functions: vec![DtalFunction {
                name: "test".to_string(),
                params: vec![(Reg::Virtual(VirtualReg(0)), DtalType::Int)],
                parameter_ownerships: vec![],
                return_type: DtalType::Int,
                precondition: None,
                postcondition: None,
                blocks: vec![DtalBlock {
                    label: ".test_bb0".to_string(),
                    entry_state: TypeState::new(),
                    instructions: vec![
                        DtalInstr::MovImm {
                            dst: Reg::Virtual(VirtualReg(1)),
                            imm: 42,
                            ty: DtalType::Int,
                        },
                        DtalInstr::MovReg {
                            dst: Reg::Physical(PhysicalReg::R0),
                            src: Reg::Virtual(VirtualReg(1)),
                            ty: DtalType::Int,
                        },
                        DtalInstr::Ret,
                    ],
                }],
            }],
        };

        // Emit → parse → emit → compare
        let text1 = emit_program(&input_program);
        let parsed = parse_dtal(&text1).expect("parse should succeed");
        let text2 = emit_program(&parsed);

        assert_eq!(text1, text2, "Round-trip should produce identical output");
    }
}
