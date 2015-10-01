module type INTERP =
  sig
    val eval : string -> string
    exception Syntax_error
    exception Runtime_error of string
  end;;



