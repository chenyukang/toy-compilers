open Interp
open Load
open While

let _ =
  Load.sample := "
    fact := 1 ;
    val := 10000 ;
    cur := val ;
    mod := 1000000007 ;
    while ( cur > 1 )
      do
       {
          fact := fact * cur ;
          fact := fact - fact / mod * mod ;
          cur := cur - 1;
          put cur
       } ;
    cur := 0;
    put  val";
  let module Loader = Loader(WhileInterp) in 
  Html.window##onload <- Html.handler (Loader.start)
