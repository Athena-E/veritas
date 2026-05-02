use vstd::prelude::*;

verus! {

fn read_offset(i: int) -> (x: int)
    requires 0 <= i && i < 4,
    ensures x == 7,
{
    let arr = [7int, 7int, 7int, 7int, 7int];
    arr[(i + 1) as usize]
}

fn main() {
    let _ = read_offset(2);
}

}
