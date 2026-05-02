predicate Sorted(arr: array<int>)
  reads arr
{
  forall i, j :: 0 <= i <= j < arr.Length ==> arr[i] <= arr[j]
}

method BinarySearch(arr: array<int>, target: int) returns (idx: int)
  requires arr.Length == 5
  requires Sorted(arr)
  ensures idx == -1 || 0 <= idx < arr.Length
  ensures idx != -1 ==> arr[idx] == target
{
  var lo := 0;
  var hi := arr.Length;
  while lo < hi
    invariant 0 <= lo <= hi <= arr.Length
    invariant forall i :: 0 <= i < lo ==> arr[i] < target
    invariant forall i :: hi <= i < arr.Length ==> arr[i] > target
  {
    var mid := lo + (hi - lo) / 2;
    if arr[mid] < target {
      lo := mid + 1;
    } else if arr[mid] > target {
      hi := mid;
    } else {
      idx := mid;
      return;
    }
  }
  idx := -1;
}

method Main() returns (r: int)
{
  var arr := new int[5];
  arr[0] := 1;
  arr[1] := 3;
  arr[2] := 5;
  arr[3] := 7;
  arr[4] := 9;
  r := BinarySearch(arr, 7);
}
