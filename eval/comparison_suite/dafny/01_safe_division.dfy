method SafeDivide(a: int, b: int) returns (q: int)
  requires b != 0
{
  q := a / b;
}

method Main() returns (r: int)
{
  r := SafeDivide(10, 2);
}
