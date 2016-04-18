open Interp
open While_func

let _ =
  let sample = "\
def addx num1, num2
    res := num1 + num2;
    put res;
    return res * 2
end;

res := addx 1, 2;
put res
"; in
  let res = WhileFuncInterp.eval sample in
  Printf.printf "res: %s\n" res

