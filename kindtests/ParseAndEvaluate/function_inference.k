function1 (i) { i + 22; }
function2 () { 20; }

test ()
{
  function1 (function2 ());
}
