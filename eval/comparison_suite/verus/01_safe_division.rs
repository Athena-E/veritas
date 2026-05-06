use vstd::prelude::*;

verus! {

fn safe_divide(a: u32, b: u32) -> (q: u32)
    requires b != 0,
{
    a / b
}

fn main() {
    let _ = safe_divide(10, 2);
}

}
