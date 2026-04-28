#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OwnershipMode {
    Plain,
    Consume,
    FreshOwned,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParameterKind {
    PlainValue,
    OwnedValue,
    SharedBorrow,
    MutableBorrow,
}

impl OwnershipMode {
    pub fn consumes_input(self) -> bool {
        matches!(self, Self::Consume)
    }

    pub fn produces_owned_output(self) -> bool {
        matches!(self, Self::FreshOwned)
    }
}

impl ParameterKind {
    pub fn is_plain_value(self) -> bool {
        matches!(self, Self::PlainValue)
    }

    pub fn is_owned_value(self) -> bool {
        matches!(self, Self::OwnedValue)
    }

    pub fn is_shared_borrow(self) -> bool {
        matches!(self, Self::SharedBorrow)
    }

    pub fn is_mutable_borrow(self) -> bool {
        matches!(self, Self::MutableBorrow)
    }

    pub fn is_borrow(self) -> bool {
        self.is_shared_borrow() || self.is_mutable_borrow()
    }
}
