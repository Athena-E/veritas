use super::DtalParseError;
use crate::dtal::constraints::{Constraint, IndexExpr};
use crate::dtal::instr::CmpOp;
use crate::dtal::regs::{PhysicalReg, Reg, VirtualReg};
use crate::dtal::types::DtalType;
use std::sync::Arc;

pub(super) fn parse_reg(s: &str) -> Option<Reg> {
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

pub(super) fn parse_cmpop(s: &str) -> Option<CmpOp> {
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

pub(super) fn split_instruction_comment(line: &str) -> (&str, Option<&str>) {
    if let Some(pos) = line.find("    : ") {
        let instr = line[..pos].trim();
        let comment = line[pos + 6..].trim();
        return (
            instr,
            if comment.is_empty() {
                None
            } else {
                Some(comment)
            },
        );
    }
    if let Some(pos) = line.find("    ;") {
        let instr = line[..pos].trim();
        let comment = line[pos + 5..].trim();
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

pub(super) fn parse_type_str(s: &str) -> Result<DtalType, DtalParseError> {
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
            let rest = &s[7..];
            let dot = rest
                .find('.')
                .ok_or_else(|| err_static(format!("expected '.' in existential type '{}'", s)))?;
            let witness_var = rest[..dot].trim().to_string();
            let after_dot = rest[dot + 1..].trim();
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

pub(super) fn parse_constraint_str(s: &str) -> Result<Constraint, DtalParseError> {
    let s = s.trim();
    if s == "true" {
        return Ok(Constraint::True);
    }
    if s == "false" {
        return Ok(Constraint::False);
    }

    if s.starts_with('(') && s.ends_with(')') {
        let inner = &s[1..s.len() - 1];

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

        if inner.starts_with("forall ") || inner.starts_with("exists ") {
            return parse_quantifier(inner);
        }

        return parse_constraint_str(inner);
    }

    if let Some(rest) = s.strip_prefix('!') {
        let inner = parse_constraint_str(rest)?;
        return Ok(Constraint::Not(Box::new(inner)));
    }

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

pub(super) fn parse_index_expr(s: &str) -> Result<IndexExpr, DtalParseError> {
    let s = s.trim();

    if s.starts_with('(') && s.ends_with(')') {
        let inner = &s[1..s.len() - 1];
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

    if let Some(bracket) = s.find('[')
        && s.ends_with(']')
    {
        let name = s[..bracket].to_string();
        let idx_str = &s[bracket + 1..s.len() - 1];
        let idx = parse_index_expr(idx_str)?;
        return Ok(IndexExpr::Select(name, Box::new(idx)));
    }

    if let Ok(n) = s.parse::<i128>() {
        return Ok(IndexExpr::Const(n));
    }

    if s.starts_with('-')
        && let Ok(n) = s[1..].parse::<i128>()
    {
        return Ok(IndexExpr::Const(-n));
    }

    if s.chars().all(|c| c.is_alphanumeric() || c == '_') {
        return Ok(IndexExpr::Var(s.to_string()));
    }

    Err(err_static(format!("cannot parse index expr '{}'", s)))
}

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
            if op == "<" && i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                i += 1;
                continue;
            }
            if op == ">" && i + 1 < bytes.len() && bytes[i + 1] == b'=' {
                i += 1;
                continue;
            }
            if op == ">" && i > 0 && bytes[i - 1] == b'=' {
                i += 1;
                continue;
            }
            if op == "==" && i > 0 && bytes[i - 1] == b'!' {
                i += 1;
                continue;
            }
            if op == "==" && i + 2 < bytes.len() && bytes[i + 2] == b'>' {
                i += 1;
                continue;
            }
            return Some(i);
        }
        i += 1;
    }
    None
}

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

fn find_top_level_arith(s: &str, op: &str) -> Option<usize> {
    find_top_level_op(s, op)
}

pub(super) fn find_top_level_char(s: &str, ch: char) -> Option<usize> {
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

pub(super) fn split_top_level(s: &str, sep: char) -> Vec<&str> {
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
