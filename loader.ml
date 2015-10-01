open Interp
open Brain

module Loader (InterpImpl : INTERP) = struct
  let start sample =
    InterpImpl.eval sample;;
end;;

let run =
  let module Loader = Loader(BrainFuck) in
  let res = Loader.start("++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.") in
  Printf.printf "%s" res;;



