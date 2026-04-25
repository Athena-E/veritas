//! TIR instructions and terminators
//!
//! This module defines the instruction set for TIR (Typed Intermediate Representation).

use crate::backend::dtal::{Constraint, VirtualReg};
use crate::backend::tir::types::{BinaryOp, BlockId, UnaryOp};
use crate::common::ownership::OwnershipMode;
use crate::common::types::IType;

/// TIR instructions (three-address code in SSA form)
#[derive(Clone, Debug)]
pub enum TirInstr<'src> {
    /// dst = immediate
    LoadImm {
        dst: VirtualReg,
        value: i64,
        ty: IType<'src>,
    },

    /// dst = src (copy)
    Copy {
        dst: VirtualReg,
        src: VirtualReg,
        ty: IType<'src>,
    },

    /// dst = move src (ownership transfer)
    MoveOwned {
        dst: VirtualReg,
        src: VirtualReg,
        ty: IType<'src>,
    },

    /// dst = lhs op rhs
    BinOp {
        dst: VirtualReg,
        op: BinaryOp,
        lhs: VirtualReg,
        rhs: VirtualReg,
        ty: IType<'src>,
    },

    /// dst = op operand
    UnaryOp {
        dst: VirtualReg,
        op: UnaryOp,
        operand: VirtualReg,
        ty: IType<'src>,
    },

    /// dst = base[index]
    ArrayLoad {
        dst: VirtualReg,
        base: VirtualReg,
        index: VirtualReg,
        element_ty: IType<'src>,
        bounds_constraint: Constraint,
    },

    /// base[index] = value
    ArrayStore {
        base: VirtualReg,
        index: VirtualReg,
        value: VirtualReg,
        bounds_constraint: Constraint,
    },

    /// dst = call func(args...)
    Call {
        dst: Option<VirtualReg>,
        func: String,
        args: Vec<VirtualReg>,
        arg_ownerships: Vec<OwnershipMode>,
        ownership: OwnershipMode,
        result_ty: IType<'src>,
    },

    /// Allocate array on stack
    AllocArray {
        dst: VirtualReg,
        element_ty: IType<'src>,
        size: i64,
        region: Option<VirtualReg>,
    },

    /// Enter a nested lexical region, yielding a region handle.
    RegionEnter {
        dst: VirtualReg,
    },

    /// Leave a nested lexical region.
    RegionLeave {
        region: VirtualReg,
    },

    /// Assume a constraint (from branch or precondition)
    AssumeConstraint { constraint: Constraint },

    /// Assert a constraint (proven by frontend, carried for verification)
    AssertConstraint { constraint: Constraint },
}

impl<'src> TirInstr<'src> {
    /// Get the destination register of this instruction (if any)
    pub fn dst(&self) -> Option<VirtualReg> {
        match self {
            TirInstr::LoadImm { dst, .. } => Some(*dst),
            TirInstr::Copy { dst, .. } => Some(*dst),
            TirInstr::MoveOwned { dst, .. } => Some(*dst),
            TirInstr::BinOp { dst, .. } => Some(*dst),
            TirInstr::UnaryOp { dst, .. } => Some(*dst),
            TirInstr::ArrayLoad { dst, .. } => Some(*dst),
            TirInstr::ArrayStore { .. } => None,
            TirInstr::Call { dst, .. } => *dst,
            TirInstr::AllocArray { dst, .. } => Some(*dst),
            TirInstr::RegionEnter { dst } => Some(*dst),
            TirInstr::RegionLeave { .. } => None,
            TirInstr::AssumeConstraint { .. } => None,
            TirInstr::AssertConstraint { .. } => None,
        }
    }

    /// Get the type of the result (if any)
    pub fn result_type(&self) -> Option<&IType<'src>> {
        match self {
            TirInstr::LoadImm { ty, .. } => Some(ty),
            TirInstr::Copy { ty, .. } => Some(ty),
            TirInstr::MoveOwned { ty, .. } => Some(ty),
            TirInstr::BinOp { ty, .. } => Some(ty),
            TirInstr::UnaryOp { ty, .. } => Some(ty),
            TirInstr::ArrayLoad { element_ty, .. } => Some(element_ty),
            TirInstr::ArrayStore { .. } => None,
            TirInstr::Call { result_ty, .. } => Some(result_ty),
            TirInstr::AllocArray { element_ty, .. } => Some(element_ty),
            TirInstr::RegionEnter { .. } => None,
            TirInstr::RegionLeave { .. } => None,
            TirInstr::AssumeConstraint { .. } => None,
            TirInstr::AssertConstraint { .. } => None,
        }
    }
}

/// Block terminator
#[derive(Clone, Debug)]
pub enum Terminator {
    /// Unconditional jump
    Jump { target: BlockId },

    /// Conditional branch
    Branch {
        cond: VirtualReg,
        true_target: BlockId,
        false_target: BlockId,
        /// Constraint added to true branch
        true_constraint: Box<Constraint>,
        /// Constraint added to false branch
        false_constraint: Box<Constraint>,
    },

    /// Return from function
    Return {
        value: Option<VirtualReg>,
        ownership: OwnershipMode,
    },

    /// Unreachable (after error or infinite loop)
    Unreachable,
}
