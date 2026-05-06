predicate Sorted3(arr: array<int>)
  reads arr
{
  arr.Length == 3 &&
  forall i, j :: 0 <= i <= j < arr.Length ==> arr[i] <= arr[j]
}

method SortedHead(arr: array<int>) returns (x: int)
  requires Sorted3(arr)
{
  x := arr[0];
}

method MakeSorted() returns (arr: array<int>)
  ensures Sorted3(arr)
{
  arr := new int[3];
  arr[0] := 1;
  arr[1] := 2;
  arr[2] := 3;
}

method Main() returns (r: int)
{
  var arr := MakeSorted();
  r := SortedHead(arr);
}
