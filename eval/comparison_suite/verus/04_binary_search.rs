use vstd::prelude::*;

verus! {

spec fn sorted(a: Seq<i32>) -> bool {
    forall|i: int, j: int| 0 <= i <= j < a.len() ==> a[i] <= a[j]
}

fn binary_search(arr: [i32; 10], target: i32) -> (idx: i32)
    requires sorted(arr@),
{
    let mut lo: usize = 0;
    let mut hi: usize = 10;
    while lo < hi
        invariant
            lo <= hi <= 10,
        decreases hi - lo
    {
        let mid = lo + (hi - lo) / 2;
        let v = arr[mid];
        if v < target {
            lo = mid + 1;
        } else if v > target {
            hi = mid;
        } else {
            return mid as i32;
        }
    }
    -1
}

fn main() {
    let arr: [i32; 10] = [3, 7, 12, 15, 22, 34, 41, 55, 68, 90];
    let _ = binary_search(arr, 34);
}

}
