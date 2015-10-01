open Interp
module Html = Dom_html

let elem_from_id id =
  let elem =
    Js.Opt.get (Html.document##getElementById(Js.string id))
               (fun () -> assert false) in
  elem;;


let sample =
  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.";;


let start sample (interp_impl:INTERP) =
  let wrapper = elem_from_id "textarea_wrapper" in
  let button = elem_from_id "compile_button" in
  let out_wrapper = elem_from_id "output" in
  let source  = Html.createTextarea Html.document in
  let result = Html.createTextarea Html.document in
  source##style##width <- Js.string "100%";
  source##style##height <- Js.string "100%";
  source##style##padding <- Js.string "8px";
  source##value <- (Js.string sample);

  result##style##width <- Js.string "100%";
  result##style##height <- Js.string "100%";
  result##style##padding <- Js.string "8px";

  Dom.appendChild wrapper source;
  Dom.appendChild out_wrapper result;
  button##onclick <- Html.handler (
                         (fun _ -> (
                            let v = Js.to_string (source##value) in
                            let r = interp_impl.eval v "" in
                            result##value <- (Js.string r);
                            Js._true)));
  Js._false

let _ =
  let sample =
  "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++." in 
  Html.window##onload <- Html.handler (start sample)


