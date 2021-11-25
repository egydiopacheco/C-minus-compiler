open Pp
    
let parse = 
  print_string "AST - PrettyPrinting \n\n";
  let prog = open_in Sys.argv.(1) in
  let lexbuf = Lexing.from_channel prog in
  let ast = Parser.program Lexer.token lexbuf in
  pretty_print ast
