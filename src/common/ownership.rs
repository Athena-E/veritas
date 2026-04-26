#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OwnershipMode {
    Plain,
    Consume,
    FreshOwned,
}

impl OwnershipMode {
    pub fn consumes_input(self) -> bool {
        matches!(self, Self::Consume)
    }

    pub fn produces_owned_output(self) -> bool {
        matches!(self, Self::FreshOwned)
    }
}
