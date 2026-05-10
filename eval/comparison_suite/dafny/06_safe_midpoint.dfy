method SafeMidpoint(lo: int, hi: int) returns (m: int)
  requires lo <= hi
  ensures lo <= m <= hi
{
  m := lo + (hi - lo) / 2;
}

method Main() returns (r: int)
{
  r := SafeMidpoint(10, 20);
}
