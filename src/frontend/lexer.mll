{ 

open Parser

let position lexbuf =
    let p = lexbuf.Lexing.lex_curr_p in
    Printf.sprintf "\nError found in: %s \nat line : %d \nat column : %d\n" 
        p.Lexing.pos_fname p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol)

let set_filename (fname:string) (lexbuf:Lexing.lexbuf)  =
    ( lexbuf.Lexing.lex_curr_p <-  
        { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname }
    ; lexbuf
    )

exception Error of string
let error lexbuf fmt = 
    Printf.kprintf (fun msg -> 
        raise (Error ((position lexbuf) ^""^ msg))) fmt

}

(* Regular expressions definitions *)
let digit      = ['0'-'9']
let letter     = ['a'-'z' 'A'-'Z']
let number     = '-'? digit+
let id         = (letter) (letter|digit|'_')*
let whitespace = [' ' '\t']+
let newline    = '\r' | '\n' | "\r\n"

(* Lexing Rules *)
rule token = 
    parse
    | whitespace { token lexbuf }
    | newline   { Lexing.new_line lexbuf; token lexbuf }
    | "/*"      { match_comment lexbuf }
    | '<'       { LT }
    | "!="      { NEQ }
    | '>'       { GT }
    | "<="      { LEQ }
    | ">="      { GEQ }
    | '='       { ASSIGN }
    | "=="      { EQ }
    | ','       { COMMA }
    | ';'       { SEMI }
    | '('       { LPAREN }
    | ')'       { RPAREN }
    | '['       { LBRACKET }
    | ']'       { RBRACKET }
    | '{'       { LBRACE }
    | '}'       { RBRACE }
    | '-'       { MINUS }
    | '+'       { PLUS }
    | '/'       { DIV }
    | '*'       { TIMES }
    | "if"      { IF }
    | "else"    { ELSE }
    | "void"    { VOID_TYPE }
    | "int"     { INT_TYPE  }
    | "while"   { WHILE }
    | "return"  { RETURN }
    | id        { ID (Lexing.lexeme lexbuf) }
    | number    { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | eof       { EOF }
    | _         { error lexbuf "The lexeme ('%s') is meaningless to C- language." (Lexing.lexeme lexbuf) }

and match_comment =
    parse
    | "*/"      { token lexbuf }
    | newline   { Lexing.new_line lexbuf; match_comment lexbuf }
    | eof       { error lexbuf "Comments must be closed." (Lexing.lexeme lexbuf) }
    | _         { match_comment lexbuf }

