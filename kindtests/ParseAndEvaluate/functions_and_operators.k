function1 () : int { 24; }
function2 (v:int) : int { v + 10; }

test ()
{
  var1 := function1 ();
  var1 + function2 (8);
}
