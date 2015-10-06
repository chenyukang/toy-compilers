open Interp
open Load
open While_func

let _ =
  sample := "
def func arg1, arg2
    put arg1;
    put arg2;
    put arg1 + arg2
end;

def demo arg1
    while (arg1 < 10) do {
          put arg1;
          arg1 := arg1 + 1
    };
    put arg1
end;

put func;
put demo;
demo 1";
  let module Loader = Loader(WhileFuncInterp) in 
  Html.window##onload <- Html.handler (Loader.start)

