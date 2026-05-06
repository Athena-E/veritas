predicate Sorted(arr: array<int>)
  reads arr
{
  forall i, j :: 0 <= i <= j < arr.Length ==> arr[i] <= arr[j]
}

method BinarySearch(arr: array<int>, target: int) returns (idx: int)
  requires arr.Length == 10
  requires Sorted(arr)
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
  var arr := new int[10];
  arr[0] := 3;
  arr[1] := 7;
  arr[2] := 12;
  arr[3] := 15;
  arr[4] := 22;
  arr[5] := 34;
  arr[6] := 41;
  arr[7] := 55;
  arr[8] := 68;
  arr[9] := 90;
  r := BinarySearch(arr, 34);
}
