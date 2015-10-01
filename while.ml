type token = Number of int
           | Variable of string
           | Bool of bool
           | KwdIf
           | KwdThen
           | KwdElse
           | KwdWhile
           | KwdDo
           | LParen
           | RParen
           | LBracket
           | RBracket
           | OpPlus
           | OpSub
           | OpMul
           | OpDiv
           | OpOr
           | OpAnd
           | OpAssign
           | OpLess
           | OpLarg
           | OpPut
           | Semi;;

type exp =
  | VarExp of string
  | BoolExp of bool
  | ValExp of int
  | PlusExp of exp * exp
  | SubExp of exp * exp
  | MulExp of exp * exp
  | DivExp of exp * exp
  | OrExp of exp * exp
  | AndExp of exp * exp
  | LeExp of exp * exp
  | LaExp of exp * exp
 and stmt =
   | Stmts of stmt list
   | Assign of exp * exp
   | IfStmt of exp * stmt * stmt
   | PrintStmt of exp
   | WhileStmt of exp * stmt;;

type program = stmt list;;

exception Invalid_char of char
exception Syntax_error
exception Runtime_error

let rec lexer_of_channel channel =
  let src = Stream.of_channel channel in
  Stream.from (fun _ -> lex src)

and lexer_of_string str =
  let src = Stream.of_string str in
  Stream.from (fun _ -> lex src)

and lex stream =
  if Stream.peek stream = None then None
  else (
    match Stream.next stream with
    (* skip whitespace *)
    | ' ' | '\t' | '\r' | '\n' -> lex stream
    (* assign *)
    | ':' when Stream.peek stream = Some '=' ->
       Stream.junk stream;
       Some OpAssign
    (* identifier *)
    | ('A'..'Z' | 'a'..'z' as ch) ->
       let buffer = Buffer.create 20 in
       Buffer.add_char buffer ch;
       Some(lex_ident stream buffer)
    (* read number *)
    | '0'..'9' as ch ->
       let buffer = Buffer.create 20 in
       Buffer.add_char buffer ch;
       Some(lex_num stream buffer)
    | ';' -> Some(Semi)
    | '(' -> Some LParen
    | ')' -> Some RParen
    | '{' -> Some LBracket
    | '}' -> Some RBracket
    | '+' -> Some OpPlus
    | '-' -> Some OpSub
    | '*' -> Some OpMul
    | '/' -> Some OpDiv
    | '>' -> Some OpLarg
    | '<' -> Some OpLess
    | _ as ch -> Printf.printf "invalid: %c" ch; raise (Invalid_char ch)
  )

and lex_ident stream buffer =
  match Stream.peek stream with
  | Some('A'..'Z' | 'a'..'z' | '0'..'9' | ':' | '=' as ch) ->
     Buffer.add_char buffer ch;
     Stream.junk stream;
     lex_ident stream buffer
  | _ ->
     let ident = buffer |> Buffer.contents |> String.lowercase in (
       match ident with
       | "if" -> KwdIf
       | "then" -> KwdThen
       | "do" -> KwdDo
       | "while" -> KwdWhile
       | "and" -> OpAnd
       | "or" -> OpOr
       | "put" -> OpPut
       | "true" -> Bool true
       | "false" -> Bool false
       | _ -> Variable ident)

and lex_num stream buffer =
  match Stream.peek stream with
  | Some('0'..'9' as ch) ->
     Buffer.add_char buffer ch;
     Stream.junk stream;
     lex_num stream buffer
  | _ -> Number (buffer |> Buffer.contents |> int_of_string)

let tokens stream =
  let rec loop() =
    if Stream.peek stream = None then []
    else (
      match Stream.peek stream with
      | Some x -> (Stream.junk stream; (x) :: loop())
      | _ -> raise Syntax_error) in
  loop();;

(* parser *)
let rec parse stream = parse_stmts stream

and skip stream tokens =
  List.iter (fun t ->
             match Stream.peek stream with
             | Some t when t = t -> Stream.junk stream
             | _ -> raise Syntax_error)
            tokens

and parse_stmts stream =
  let stmt = parse_stmt stream in
  let rec loop() =
    if Stream.peek stream = None then []
    else
      match Stream.peek stream with
      | Some(Semi) ->
         Stream.junk stream;
         let stmt = parse_stmt stream in
         stmt :: (loop())
      | _ -> [] in
  let rest = loop() in
  if rest = [] then stmt else Stmts (stmt::rest)

and parse_stmt stream =
  match Stream.peek stream with
  | Some KwdIf -> parse_if stream
  | Some KwdWhile -> parse_while stream
  | Some OpPut -> parse_put stream
  | Some (Variable _) -> parse_assign stream
  | _ -> raise Syntax_error

and parse_if stream =
  skip stream [KwdIf];
  let pred = parse_exp stream in
  skip stream [KwdThen; LBracket];
  let tbody = parse_stmts stream in
  (* skip } else { *)
  skip stream [RBracket; KwdElse; LBracket];
  let fbody = parse_stmts stream in
  skip stream [RBracket];
  IfStmt(pred, tbody, fbody)

and parse_while stream =
  skip stream [KwdWhile];
  let cond = parse_exp stream in
  (* skip do { *)
  skip stream [KwdDo; LBracket];
  let body = parse_stmts stream in
  skip stream [RBracket];
  WhileStmt(cond, body)

and parse_put stream =
  skip stream [OpPut];
  let value = parse_exp stream in
  PrintStmt value

and parse_assign stream =
  let lhs = parse_var stream in
  skip stream [OpAssign];
  let rhs = parse_exp stream in
  Assign(lhs, rhs)

and parse_var stream =
  match Stream.peek stream with
  | Some(Variable id) -> Stream.junk stream; VarExp id
  | _ -> raise Syntax_error

and parse_exp stream = parse_or_exp stream

and parse_or_exp stream =
  let lhs = parse_and_exp stream in
  let rec loop lhs =
    match Stream.peek stream with
    | Some OpOr ->
       Stream.junk stream;
       let rhs = parse_and_exp stream in
       loop (OrExp (lhs, rhs))
    | _ -> lhs in
  loop lhs

and parse_and_exp stream =
  let lhs = parse_cmp_stream stream in
  let rec loop lhs =
    match Stream.peek stream with
    | Some OpAnd ->
       Stream.junk stream;
       let rhs = parse_cmp_stream stream in
       loop (AndExp (lhs, rhs))
    | _ -> lhs in
  loop lhs

and parse_cmp_stream stream =
  let lhs = parse_plus_sub_exp stream in
  let rec loop lhs =
    match Stream.peek stream with
    | Some OpLess ->
       Stream.junk stream;
       let rhs = parse_plus_sub_exp stream in
       LeExp(lhs, rhs)
    | Some OpLarg ->
       Stream.junk stream;
       let rhs = parse_plus_sub_exp stream in
       LaExp(lhs, rhs)
    | _ -> lhs in
  loop lhs

and parse_plus_sub_exp stream =
  let lhs = parse_mul_div_exp stream in
  let rec loop lhs =
    match Stream.peek stream with
    | Some OpPlus ->
       Stream.junk stream;
       let rhs = parse_mul_div_exp stream in
       loop (PlusExp(lhs, rhs))
    | Some OpSub ->
       Stream.junk stream;
       let rhs = parse_mul_div_exp stream in
       loop (SubExp(lhs, rhs))
    | _ -> lhs in
  loop lhs

and parse_mul_div_exp stream =
  let lhs = parse_primary_exp stream in
  let rec loop lhs =
    match Stream.peek stream with
    | Some OpMul ->
       Stream.junk stream;
       let rhs = parse_primary_exp stream in
       loop (MulExp(lhs, rhs))
    | Some OpDiv ->
       Stream.junk stream;
       let rhs = parse_primary_exp stream in
       loop (DivExp(lhs, rhs))
    | _ -> lhs in
  loop lhs

and parse_primary_exp stream =
  match Stream.peek stream with
  | Some LParen ->
     Stream.junk stream;
     let exp = parse_exp stream in
     Stream.junk stream;
     exp
  | Some (Variable x) ->
     Stream.junk stream;
     VarExp x
  | Some (Number x) ->
     Stream.junk stream;
     ValExp x
  | Some (Bool x) ->
     Stream.junk stream;
     BoolExp x
  | _ -> raise Syntax_error;;

let print_value value res =
  res := !res ^ (Printf.sprintf "value: %d\n" value);;

(* evaler *)
let rec eval prog env res=
  match prog with
  | Stmts [] -> ()
  | Stmts (stmt::left) ->
     eval stmt env res;
     eval (Stmts left) env res
  | Assign (lhs, rhs) ->
     let key = variable_name lhs in
     let value = eval_exp rhs env in
     Hashtbl.replace env key value
  | IfStmt (cond, tbody, fbody) ->
     if eval_bexp cond env then
       eval tbody env res
     else
       eval fbody env res
  | PrintStmt exp ->
     let r = eval_exp exp env in
     print_value r res
  | WhileStmt (cond, body) ->
     let rec loop() =
       if eval_bexp cond env then (
         eval body env res;
         loop())
       else () in
     loop()

and variable_name exp =
  match exp with
  | VarExp x -> x
  | _ -> raise Runtime_error

and eval_exp exp env =
  match exp with
  | PlusExp (lhs, rhs) ->
     let l' = eval_exp lhs env in
     let r' = eval_exp rhs env in
     l' + r'
  | SubExp (lhs, rhs) ->
     let l' = eval_exp lhs env in
     let r' = eval_exp rhs env in
     l' - r'
  | MulExp (lhs, rhs) ->
     let l' = eval_exp lhs env in
     let r' = eval_exp rhs env in
     l' * r'
  | DivExp (lhs, rhs) ->
     let l' = eval_exp lhs env in
     let r' = eval_exp rhs env in
     if r' = 0 then raise Runtime_error
     else l' / r'
  | VarExp x -> Hashtbl.find env x
  | ValExp x -> x
  | _ -> raise Runtime_error

and eval_bexp exp env =
  match exp with
  | LeExp (lhs, rhs) ->
     let l' = eval_exp lhs env in
     let r' = eval_exp rhs env in
     l' < r'
  | LaExp (lhs, rhs) ->
     let l' = eval_exp lhs env in
     let r' = eval_exp rhs env in
     l' > r'
  | AndExp (lhs, rhs) ->
     let l' = eval_bexp lhs env in
     let r' = eval_bexp rhs env in
     l' && r'
  | OrExp (lhs, rhs) ->
     let l' = eval_bexp lhs env in
     let r' = eval_bexp rhs env in
     l' || r'
  | BoolExp v -> v
  | _ -> raise Runtime_error;;

let print_of_string str =
  (lexer_of_string str) |> tokens;;

let parse_of_string str =
  (lexer_of_string str) |> parse_stmts;;

let eval_prog prog =
  let env = Hashtbl.create 1000 in
  let res = ref "" in
  eval prog env res;
  !res;;

let eval_string str =
  let stmts = (lexer_of_string str) |> parse_stmts in
  let res = eval_prog stmts in
  res;;

let main() =
  let stmts = (lexer_of_channel stdin) |> parse_stmts in
  let res = eval_prog stmts in
  Printf.printf "result: %s\n" res;;

module Html = Dom_html

let elem_from_id id =
  let elem =
    Js.Opt.get (Html.document##getElementById(Js.string id))
               (fun () -> assert false) in
  elem;;

let sample = "
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

put  val";;

let start _ =
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
                            let r = eval_string v in
                            result##value <- (Js.string r);
                            Js._true)));
  Js._false

let _ =
  Html.window##onload <- Html.handler start
