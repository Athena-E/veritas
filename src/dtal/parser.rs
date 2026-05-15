//! DTAL Text Parser
//!
//! Parses the text format emitted by the DTAL emitter back into
//! `DtalProgram`, enabling standalone verification from DTAL files.

use crate::common::ownership::{LifetimeId, OwnershipMode, ParameterKind};
use crate::dtal::instr::{BinaryOp, DtalBlock, DtalFunction, DtalInstr, DtalProgram, TypeState};
use crate::dtal::regs::Reg;
use crate::dtal::types::DtalType;

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

fn parse_optional_lifetime_token(token: &str) -> (Option<LifetimeId>, usize) {
    let trimmed = token.trim_end_matches(',');
    if let Some(rest) = trimmed.strip_prefix("'l")
        && let Ok(id) = rest.parse::<u32>()
    {
        return (Some(LifetimeId(id)), 2);
    }
    (None, 1)
}

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
            parameter_kinds: vec![ParameterKind::PlainValue; params.len()],
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
            "borrow_mut" => self.parse_borrow_mut(&tokens, ty_comment),
            "move_owned" => self.parse_move_owned(&tokens, ty_comment),
            "borrow_end" => self.parse_borrow_end(&tokens, ty_comment),
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
        let (lifetime, dst_idx) = parse_optional_lifetime_token(tokens[1]);
        let dst_str = tokens[dst_idx].trim_end_matches(',');
        let src_str = tokens[dst_idx + 1];
        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        let src =
            parse_reg(src_str).ok_or_else(|| self.err(format!("invalid src '{}'", src_str)))?;
        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);
        Ok(Some(DtalInstr::AliasBorrow {
            lifetime,
            dst,
            src,
            ty,
        }))
    }

    fn parse_borrow_mut(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        if tokens.len() < 3 {
            return Err(self.err("borrow_mut requires 2 operands"));
        }
        let (lifetime, dst_idx) = parse_optional_lifetime_token(tokens[1]);
        let dst_str = tokens[dst_idx].trim_end_matches(',');
        let src_str = tokens[dst_idx + 1];
        let dst =
            parse_reg(dst_str).ok_or_else(|| self.err(format!("invalid dst '{}'", dst_str)))?;
        let src =
            parse_reg(src_str).ok_or_else(|| self.err(format!("invalid src '{}'", src_str)))?;
        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);
        Ok(Some(DtalInstr::BorrowMut {
            lifetime,
            dst,
            src,
            ty,
        }))
    }

    fn parse_borrow_end(
        &self,
        tokens: &[&str],
        ty_comment: Option<&str>,
    ) -> Result<Option<DtalInstr>, DtalParseError> {
        if tokens.len() < 2 || tokens.len() > 3 {
            return Err(self.err("borrow_end requires exactly 1 operand"));
        }
        let (lifetime, src_idx) = parse_optional_lifetime_token(tokens[1]);
        let src = parse_reg(tokens[src_idx].trim_end_matches(','))
            .ok_or_else(|| self.err("invalid source register"))?;
        let ty = ty_comment
            .map(parse_type_str)
            .transpose()?
            .unwrap_or(DtalType::Int);
        Ok(Some(DtalInstr::BorrowEnd { lifetime, src, ty }))
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
            arg_kinds: tokens
                .get(2)
                .and_then(|token| {
                    token
                        .strip_prefix('[')
                        .and_then(|rest| rest.strip_suffix(']'))
                })
                .map(|effects| {
                    if effects.is_empty() {
                        Ok(Vec::new())
                    } else {
                        effects
                            .split(',')
                            .map(|effect| match effect {
                                "value" => Ok(ParameterKind::PlainValue),
                                "owned" => Ok(ParameterKind::OwnedValue),
                                "shared" => Ok(ParameterKind::SharedBorrow),
                                "mutable" => Ok(ParameterKind::MutableBorrow),
                                other => {
                                    Err(self
                                        .err(format!("invalid call parameter kind '{}'", other)))
                                }
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

mod syntax;

use syntax::{
    find_top_level_char, parse_cmpop, parse_constraint_str, parse_reg, parse_type_str,
    split_instruction_comment, split_top_level,
};

#[cfg(test)]
use syntax::parse_index_expr;

#[cfg(test)]
mod parser_tests;
