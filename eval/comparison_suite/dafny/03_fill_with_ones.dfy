method FillWithOnes() returns (arr: array<int>)
  ensures arr.Length == 5
  ensures forall j :: 0 <= j < arr.Length ==> arr[j] == 1
{
  arr := new int[5];
  var i := 0;
  while i < arr.Length
    invariant 0 <= i <= arr.Length
    invariant forall j :: 0 <= j < i ==> arr[j] == 1
  {
    arr[i] := 1;
    i := i + 1;
  }
}

method Main() returns (r: int)
{
  var arr := FillWithOnes();
  r := arr[0] + arr[4];
}
