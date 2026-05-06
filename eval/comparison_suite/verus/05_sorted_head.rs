use vstd::prelude::*;

verus! {

spec fn sorted3(a: Seq<i32>) -> bool {
    a.len() == 3 &&
    forall|i: int, j: int| 0 <= i <= j < a.len() ==> a[i] <= a[j]
}

fn sorted_head(arr: [i32; 3]) -> (x: i32)
    requires sorted3(arr@),
{
    arr[0]
}

fn make_sorted() -> (arr: [i32; 3])
    ensures sorted3(arr@),
{
    [1, 2, 3]
}

fn main() {
    let arr = make_sorted();
    let _ = sorted_head(arr);
}

}
