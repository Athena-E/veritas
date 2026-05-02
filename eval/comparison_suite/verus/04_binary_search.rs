use vstd::prelude::*;

verus! {

spec fn sorted(a: Seq<int>) -> bool {
    forall|i: int, j: int| 0 <= i <= j < a.len() ==> a[i] <= a[j]
}

fn binary_search(arr: [int; 10], target: int) -> (idx: int)
    requires sorted(arr@),
    ensures idx == -1 || (0 <= idx && idx < 10),
{
    let mut lo: usize = 0;
    let mut hi: usize = 10;
    while lo < hi
        invariant
            lo <= hi <= 10,
            forall|i: int| 0 <= i < lo as int ==> arr@[i] < target,
            forall|i: int| hi as int <= i < 10 ==> arr@[i] > target,
    {
        let mid = lo + (hi - lo) / 2;
        let v = arr[mid];
        if v < target {
            lo = mid + 1;
        } else if v > target {
            hi = mid;
        } else {
            return mid as int;
        }
    }
    -1
}

fn main() {
    let arr = [3int, 7int, 12int, 15int, 22int, 34int, 41int, 55int, 68int, 90int];
    let _ = binary_search(arr, 34);
}

}
