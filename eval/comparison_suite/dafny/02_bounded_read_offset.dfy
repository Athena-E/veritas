method ReadOffset(i: int) returns (x: int)
  requires 0 <= i < 4
  ensures x == 7
{
  var arr := new int[5];
  var j := 0;
  while j < 5
    invariant 0 <= j <= 5
    invariant forall k :: 0 <= k < j ==> arr[k] == 7
  {
    arr[j] := 7;
    j := j + 1;
  }
  x := arr[i + 1];
}

method Main() returns (r: int)
{
  r := ReadOffset(2);
}
