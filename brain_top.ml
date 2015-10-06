open Interp
open Load
open Brain
module Html = Dom_html

let _ =
  Load.sample := "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";
  let module Loader = Loader(BrainFuck) in
  Html.window##onload <- Html.handler (Loader.start)

