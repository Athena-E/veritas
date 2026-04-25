#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OwnershipMode {
    Plain,
    Owned,
}

impl OwnershipMode {
    pub fn is_owned(self) -> bool {
        matches!(self, Self::Owned)
    }
}
