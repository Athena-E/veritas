method SafeDivide(a: int, b: int) returns (q: int)
  requires b != 0
  ensures q == a / b
{
  q := a / b;
}

method Main() returns (r: int)
{
  r := SafeDivide(10, 2);
}
