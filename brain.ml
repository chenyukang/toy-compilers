open Interp

module BrainFuck : INTERP = struct
  exception Invalid_char of char
  exception Syntax_error
  exception Runtime_error of string 

  let rec lex stream =
    if Stream.peek stream = None then None
    else (
      match Stream.next stream with
      (* skip whitespace *)
      | '+' | '-' | '>' | '<' | '.' | '[' | ']' | ',' as c -> Some(c)
      | _ -> lex stream);;

  let lexer_of_string str =
    let src = Stream.of_string str in
    Stream.from (fun _ -> lex src);;

  let tokens stream =
    let rec loop() =
      if Stream.peek stream = None then []
      else (
        match Stream.peek stream with
        | Some x -> (Stream.junk stream; (x) :: loop())
        | _ -> raise Syntax_error) in
    loop();;

  let eval_string str input =
    let insts = lexer_of_string str |> tokens |> Array.of_list in
    let values = input in
    let len = Array.length insts in
    let mem = Array.make 1000 0 in
    let counter = ref 0 in
    let pos = ref 0  in
    let result = ref "" in
    let incr_data d =
      mem.(d) <- if mem.(d) >= 255 then 0 else mem.(d) + 1 in

    let decr_data d =
      mem.(d) <- if mem.(d) <= 0 then 255 else mem.(d) - 1 in

    let read_data d =
      let r = String.get values (!pos) in
      incr pos;
      mem.(d) <- (Char.code r) in

    let rec skip_fore d c =
      match insts.(d) with
      | ']' -> (if c = 1 then d + 1
                else skip_fore (d+1) (c-1))
      | '[' -> skip_fore (d+1) (c+1)
      | _ -> skip_fore (d+1) c in

    let rec skip_back d c =
      match insts.(d) with
      | '[' -> (if c = 1 then d + 1
                else skip_back (d-1) (c-1))
      | ']' -> skip_back (d-1)(c+1)
      | _ -> skip_back (d-1) c in

    let rec loop i d =
      if i < 0 || i >= len then ()
      else (
        if !counter >= 100000 then
          (Printf.printf "\nPROCESS TIME OUT. KILLED!!!\n")
        else (
          incr counter;
          match insts.(i) with
          | '>' -> loop (i + 1) (d + 1)
          | '<' -> loop (i + 1) (d - 1)
          | '+' -> (incr_data d); loop (i + 1) d
          | '-' -> (decr_data d); loop (i + 1) d
          | '[' -> if mem.(d) <> 0 then loop (i + 1) d
            else (incr counter; loop (skip_fore i 0) d)
          | ']' -> if mem.(d) = 0 then loop (i + 1) d
            else (incr counter; loop (skip_back i 0) d)
          | ',' -> (read_data d); loop (i + 1) d
          | '.' -> (
              let v = Printf.sprintf "%c" (Char.chr mem.(d)) in
              result := !result ^ v;
              Printf.printf "%c" (Char.chr mem.(d)); loop (i + 1) d)
          | _ -> raise Syntax_error)) in
    loop 0 0;
    !result;;

  let eval str =
    eval_string str "";;

end;;

