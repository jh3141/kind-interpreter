function1 () : int { 10; }
function2 () : int { 20; }

test ()
{
  var1 := function1;
  var2 := function2;
  var1 () + var2 () + 12;
}
