use vstd::prelude::*;

verus! {

fn safe_divide(a: int, b: int) -> (q: int)
    requires b != 0,
    ensures q == a / b,
{
    a / b
}

fn main() {
    let _ = safe_divide(10, 2);
}

}
