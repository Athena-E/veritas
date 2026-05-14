//! TIR instructions and block terminators.
//!
//! Instructions are typed, SSA-style operations over virtual registers. They
//! preserve source-level ownership and constraint information until DTAL
//! generation can turn those facts into verifier-visible annotations.
//!
//! # Example
//!
//! ```text
//! v0 = immediate 40 : int(40)
//! v1 = immediate 2  : int(2)
//! v2 = v0 + v1      : int
//! return v2
//! ```
//!
//! # Design Notes
//!
//! Ownership-changing operations are first-class variants (`MoveOwned`,
//! `DropOwned`, `BorrowShared`, `BorrowMut`, and `BorrowEnd`). Later passes
//! should not infer ownership behavior from plain copies or calls; the typed
//! frontend records that intent here.
//!
//! # Error Behavior
//!
//! This module is structural and does not validate programs itself. Invalid
//! type, ownership, or constraint combinations are rejected downstream by DTAL
//! generation and [`crate::verifier`].

use crate::common::ownership::{OwnershipMode, ParameterKind};
use crate::common::types::IType;
use crate::dtal::{Constraint, VirtualReg};
use crate::middle::tir::types::{BinaryOp, BlockId, UnaryOp};

/// Typed SSA instruction.
///
/// Most variants define at most one destination virtual register. Ownership
/// operations are explicit so later lowering stages do not infer moves,
/// borrows, or drops from source syntax.
#[derive(Clone, Debug)]
pub enum TirInstr<'src> {
    /// `dst = immediate`
    LoadImm {
        dst: VirtualReg,
        value: i64,
        ty: IType<'src>,
    },

    /// `dst = src`
    Copy {
        dst: VirtualReg,
        src: VirtualReg,
        ty: IType<'src>,
    },

    /// `dst = move src`
    MoveOwned {
        dst: VirtualReg,
        src: VirtualReg,
        ty: IType<'src>,
    },

    /// `drop src`
    DropOwned { src: VirtualReg, ty: IType<'src> },

    /// `dst = &src`
    BorrowShared {
        dst: VirtualReg,
        src: VirtualReg,
        ty: IType<'src>,
    },

    /// `dst = &mut src`
    BorrowMut {
        dst: VirtualReg,
        src: VirtualReg,
        ty: IType<'src>,
    },

    /// End the borrow held in `src`.
    BorrowEnd { src: VirtualReg, ty: IType<'src> },

    /// `dst = lhs op rhs`
    BinOp {
        dst: VirtualReg,
        op: BinaryOp,
        lhs: VirtualReg,
        rhs: VirtualReg,
        ty: IType<'src>,
    },

    /// `dst = op operand`
    UnaryOp {
        dst: VirtualReg,
        op: UnaryOp,
        operand: VirtualReg,
        ty: IType<'src>,
    },

    /// `dst = base[index]`
    ArrayLoad {
        dst: VirtualReg,
        base: VirtualReg,
        index: VirtualReg,
        element_ty: IType<'src>,
        bounds_constraint: Constraint,
    },

    /// `base[index] = value`
    ArrayStore {
        base: VirtualReg,
        index: VirtualReg,
        value: VirtualReg,
        bounds_constraint: Constraint,
    },

    /// `dst = call func(args...)`
    Call {
        dst: Option<VirtualReg>,
        func: String,
        args: Vec<VirtualReg>,
        arg_types: Vec<IType<'src>>,
        arg_kinds: Vec<ParameterKind>,
        ownership: OwnershipMode,
        result_ty: IType<'src>,
    },

    /// Allocate an array on the stack.
    AllocArray {
        dst: VirtualReg,
        element_ty: IType<'src>,
        size: i64,
        region: Option<VirtualReg>,
    },

    /// Enter a nested lexical region, yielding a region handle.
    RegionEnter { dst: VirtualReg },

    /// Leave a nested lexical region.
    RegionLeave { region: VirtualReg },

    /// Assume a branch or precondition constraint.
    AssumeConstraint { constraint: Constraint },

    /// Carry a frontend-proven constraint for verification.
    AssertConstraint { constraint: Constraint },
}

impl<'src> TirInstr<'src> {
    /// Return the destination register, if any.
    pub fn dst(&self) -> Option<VirtualReg> {
        match self {
            TirInstr::LoadImm { dst, .. } => Some(*dst),
            TirInstr::Copy { dst, .. } => Some(*dst),
            TirInstr::MoveOwned { dst, .. } => Some(*dst),
            TirInstr::DropOwned { .. } => None,
            TirInstr::BorrowShared { dst, .. } => Some(*dst),
            TirInstr::BorrowMut { dst, .. } => Some(*dst),
            TirInstr::BorrowEnd { .. } => None,
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

    /// Return the result type, if any.
    pub fn result_type(&self) -> Option<&IType<'src>> {
        match self {
            TirInstr::LoadImm { ty, .. } => Some(ty),
            TirInstr::Copy { ty, .. } => Some(ty),
            TirInstr::MoveOwned { ty, .. } => Some(ty),
            TirInstr::DropOwned { ty, .. } => Some(ty),
            TirInstr::BorrowShared { ty, .. } => Some(ty),
            TirInstr::BorrowMut { ty, .. } => Some(ty),
            TirInstr::BorrowEnd { ty, .. } => Some(ty),
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

/// Control-flow terminator for a TIR basic block.
#[derive(Clone, Debug)]
pub enum Terminator {
    /// Unconditional jump.
    Jump { target: BlockId },

    /// Conditional branch.
    Branch {
        cond: VirtualReg,
        true_target: BlockId,
        false_target: BlockId,
        /// Constraint added to the true branch.
        true_constraint: Box<Constraint>,
        /// Constraint added to the false branch.
        false_constraint: Box<Constraint>,
    },

    /// Return from the function.
    Return {
        value: Option<VirtualReg>,
        ownership: OwnershipMode,
    },

    /// Unreachable control flow.
    Unreachable,
}
