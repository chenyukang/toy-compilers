open Interp

module WhileFuncInterp : INTERP = struct
  exception Invalid_char of char
  exception Syntax_error
  exception Runtime_error of string

  type token = Number of int
             | Variable of string
             | Bool of bool
             | KwdIf
             | KwdThen
             | KwdElse
             | KwdWhile
             | KwdDo
             | KwdDef
             | KwdEnd
             | Comma
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
    | FuncExp of (string list) * stmt
  and stmt =
    | FuncDef of exp * (string list) * stmt
    | Stmts of stmt list
    | Assign of exp * exp
    | IfStmt of exp * stmt * stmt
    | PrintStmt of exp
    | WhileStmt of exp * stmt
    | FuncCall of exp * (exp list);;


  type program = stmt list;;


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
      | ',' -> Some Comma
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
        | "def" -> KwdDef
        | "end" -> KwdEnd
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
    List.iter (fun _ ->
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
    | Some (Variable _) -> parse_assign_or_call stream
    | Some KwdEnd -> parse_assign_or_call stream
    | Some KwdDef -> parse_func stream
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

  and parse_func stream =
    skip stream [KwdDef];
    let name = parse_var stream in
    let args = parse_args stream in
    let body = parse_stmts stream in
    skip stream [KwdEnd];
    FuncDef(name, args, body)

  and parse_args stream =
    let args = ref [] in
    let rec loop now =
      match Stream.peek stream with
      | Some (Variable x) -> (
          if now = 0 then (
            args := !args @ [x];
            skip stream [Variable x];
            loop 1;
          )
        )
      | Some Comma -> skip stream [Comma]; (loop 0)
      | _ -> () in
    loop 0;
    !args

  and parse_put stream =
    skip stream [OpPut];
    let value = parse_exp stream in
    PrintStmt value

  and parse_assign_or_call stream =
    let lhs = parse_var stream in
    match Stream.peek stream with
    | Some OpAssign -> parse_assign lhs stream
    | _ -> parse_call lhs stream

  and parse_assign lhs stream =
    skip stream [OpAssign];
    let rhs = parse_exp stream in
    Assign(lhs, rhs)

  and parse_call lhs stream =
    let args = ref [] in
    let rec loop now =
      match Stream.peek stream with
      | Some Comma -> skip stream [Comma]; (loop 0)
      | _ -> (
          if now = 0 then (
            let arg = parse_exp stream in
            args := !args @ [arg];
            loop 1;
          )
        ) in
    loop 0;
    FuncCall(lhs, !args)

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
    let loop lhs =
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
    match value with
    | ValExp v ->
      res := !res ^ (Printf.sprintf "value: %d\n" v)
    | FuncExp(args, _) ->
      res := !res ^ (Printf.sprintf "func: with %d args\n" (List.length args))
    | _ -> Printf.printf "type error";;

  (* evaler *)
  let rec eval prog env res =
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
      if as_bool (eval_bexp cond env) then
        eval tbody env res
      else
        eval fbody env res
    | PrintStmt exp ->
      let r = eval_exp exp env in
      print_value r res
    | WhileStmt (cond, body) ->
      let rec loop() =
        if as_bool (eval_bexp cond env) then
          (eval body env res; loop()) in
      loop()
    | FuncDef (name, args, body) ->
      eval_func name args body env
    | FuncCall (func, args) ->
      let env' = Hashtbl.copy env in
      eval_call func args env' res

  and eval_call func args env res =
    match eval_exp func env with
    | FuncExp(vars, body) -> (
        let env' = extend env vars args in
        eval body env' res
      )
    | _ -> raise (Runtime_error "eval_call")

  and extend env vars args =
    let rec loop env vars args =
      match vars, args with
      | [], [] -> env
      | x::x', v::v' -> (
          Hashtbl.replace env x (eval_exp v env);
          loop env x' v'
        )
      | _, _ -> raise (Runtime_error "extend")
    in
    loop env vars args

  and variable_name exp =
    match exp with
    | VarExp x -> x
    | _ -> raise (Runtime_error "variable_name")

  and as_int exp =
    match exp with
    | ValExp v -> v
    | _ -> raise (Runtime_error "as_int")

  and as_bool exp =
    match exp with
    | BoolExp v -> v
    | _ -> raise (Runtime_error "as_bool")

  and eval_exp exp env =
    match exp with
    | PlusExp (lhs, rhs) ->
      let l' = as_int (eval_exp lhs env) in
      let r' = as_int (eval_exp rhs env) in
      ValExp (l' + r')
    | SubExp (lhs, rhs) ->
      let l' = as_int (eval_exp lhs env) in
      let r' = as_int (eval_exp rhs env) in
      ValExp (l' - r')
    | MulExp (lhs, rhs) ->
      let l' = as_int (eval_exp lhs env) in
      let r' = as_int (eval_exp rhs env) in
      ValExp (l' * r')
    | DivExp (lhs, rhs) ->
      let l' = as_int (eval_exp lhs env) in
      let r' = as_int (eval_exp rhs env) in
      if r' = 0 then raise (Runtime_error "eval_exp")
      else ValExp(l' / r')
    | VarExp x -> Hashtbl.find env x
    | ValExp _ -> exp
    | _ -> raise (Runtime_error "eval_exp other")

  and eval_func name args body env =
    let name' = variable_name name in
    Hashtbl.replace env name' (FuncExp(args, body))

  and eval_bexp exp env =
    match exp with
    | LeExp (lhs, rhs) ->
      let l' = as_int (eval_exp lhs env) in
      let r' = as_int (eval_exp rhs env) in
      BoolExp (l' < r')
    | LaExp (lhs, rhs) ->
      let l' = as_int (eval_exp lhs env) in
      let r' = as_int (eval_exp rhs env) in
      BoolExp (l' > r')
    | AndExp (lhs, rhs) ->
      let l' = as_bool (eval_bexp lhs env) in
      let r' = as_bool (eval_bexp rhs env) in
      BoolExp (l' && r')
    | OrExp (lhs, rhs) ->
      let l' = as_bool (eval_bexp lhs env) in
      let r' = as_bool (eval_bexp rhs env) in
      BoolExp (l' || r')
    | BoolExp _ -> exp
    | _ -> raise (Runtime_error "eval_bexp")

  let print_of_string str =
    (lexer_of_string str) |> tokens;;

  let parse_of_string str =
    (lexer_of_string str) |> parse_stmts;;

  let eval_prog prog =
    let env = Hashtbl.create 1000 in
    let res = ref "" in
    eval prog env res;
    !res;;

  let eval str =
    try
      (lexer_of_string str) |> parse_stmts |> eval_prog
    with
    | Runtime_error e -> Printf.sprintf "Runtime_error: %s" e
    | Syntax_error -> "Syntax_error"
    | Invalid_char c -> Printf.sprintf "Invalid_char: %c" c
    | _ -> "Error"
end;;


