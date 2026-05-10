use vstd::prelude::*;

verus! {

spec fn all_ones(a: Seq<i32>) -> bool {
    forall|i: int| 0 <= i < a.len() ==> a[i] == 1
}

fn fill_with_ones() -> (arr: [i32; 5])
    ensures all_ones(arr@),
{
    let mut arr: [i32; 5] = [0, 0, 0, 0, 0];
    let mut i: usize = 0;
    while i < 5
        invariant
            i <= 5,
            forall|j: int| 0 <= j < i ==> arr@[j] == 1,
        decreases 5usize - i
    {
        arr[i] = 1;
        i += 1;
    }
    arr
}

fn main() {
    let arr = fill_with_ones();
    let _ = arr[0] + arr[4];
}

}
