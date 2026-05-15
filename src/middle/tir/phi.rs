//! SSA phi nodes for TIR control-flow joins.
//!
//! Phi nodes define the value and type available at a block entry based on the
//! predecessor edge taken to reach that block.
//!
//! # Example
//!
//! ```text
//! bb3:
//!   v7 = phi int [bb1: v4, bb2: v6]
//! ```
//!
//! # Design Notes
//!
//! Loop-counter phis may carry an existential constraint so a widened join type
//! can still expose the invariant needed by DTAL generation and verification.

use crate::common::types::IType;
use crate::dtal::VirtualReg;
use crate::dtal::constraints::Constraint;
use crate::middle::tir::types::BlockId;

#[derive(Clone, Debug)]
pub struct PhiNode<'src> {
    pub dst: VirtualReg,
    pub ty: IType<'src>,
    pub incoming: Vec<(BlockId, VirtualReg)>,
    pub existential_constraint: Option<(String, Constraint)>,
}

impl<'src> PhiNode<'src> {
    pub fn new(dst: VirtualReg, ty: IType<'src>) -> Self {
        Self {
            dst,
            ty,
            incoming: Vec::new(),
            existential_constraint: None,
        }
    }

    pub fn add_incoming(&mut self, block: BlockId, reg: VirtualReg) {
        self.incoming.push((block, reg));
    }

    pub fn incoming_count(&self) -> usize {
        self.incoming.len()
    }
}
