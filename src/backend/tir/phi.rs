//! SSA Phi nodes
//!
//! This module defines phi nodes for SSA form, which merge values
//! at control flow join points.

use crate::backend::dtal::VirtualReg;
use crate::backend::tir::types::BlockId;
use crate::common::types::IType;

/// SSA Phi node: merges values from predecessor blocks
#[derive(Clone, Debug)]
pub struct PhiNode<'src> {
    /// The SSA variable being defined
    pub dst: VirtualReg,
    /// Type of the result (join of incoming types)
    pub ty: IType<'src>,
    /// Incoming values: (predecessor block, SSA variable from that block)
    pub incoming: Vec<(BlockId, VirtualReg)>,
}

impl<'src> PhiNode<'src> {
    /// Create a new phi node
    pub fn new(dst: VirtualReg, ty: IType<'src>) -> Self {
        Self {
            dst,
            ty,
            incoming: Vec::new(),
        }
    }

    /// Add an incoming edge
    pub fn add_incoming(&mut self, block: BlockId, reg: VirtualReg) {
        self.incoming.push((block, reg));
    }

    /// Get the number of incoming edges
    pub fn incoming_count(&self) -> usize {
        self.incoming.len()
    }
}
