use vstd::prelude::*;

verus! {

fn safe_midpoint(lo: u32, hi: u32) -> (m: u32)
    requires lo <= hi,
    ensures lo <= m && m <= hi,
{
    lo + (hi - lo) / 2
}

fn main() {
    let _ = safe_midpoint(10, 20);
}

}
