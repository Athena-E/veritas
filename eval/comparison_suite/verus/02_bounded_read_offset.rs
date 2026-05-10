use vstd::prelude::*;

verus! {

fn read_offset(i: usize) -> (x: i32)
    requires i < 4,
    ensures x == 7,
{
    let arr: [i32; 5] = [7, 7, 7, 7, 7];
    arr[i + 1]
}

fn main() {
    let _ = read_offset(2);
}

}
