{ (* Begin of header *)

(* Token type *)

    type token =
        | COMMA
        | SEMICOLON
        | LPAREN
        | RPAREN
        | LBRACKETS
        | RBRACKETS
        | LBRACE
        | RBRACE
        | LT
        | LEQ
        | GT
        | GEQ
        | NEQUAL
        | EQ
        | ASSIGN
        | MINUS
        | PLUS
        | DIV
        | MULT
        | IF
        | ELSE
        | VOID
        | WHILE
        | RETURN
        | INT of int
        | ID of string
        | STRING of string
        | EOF

let position lexbuf =
    let p = lexbuf.Lexing.lex_curr_p in
        Printf.sprintf "%s:%d:%d" 
        p.Lexing.pos_fname p.Lexing.pos_lnum (p.Lexing.pos_cnum - p.Lexing.pos_bol)

let set_filename (fname:string) (lexbuf:Lexing.lexbuf)  =
    ( lexbuf.Lexing.lex_curr_p <-  
        { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname }
    ; lexbuf
    )

exception Error of string
let error lexbuf fmt = 
    Printf.kprintf (fun msg -> 
        raise (Error ((position lexbuf)^" "^msg))) fmt

} (* end of header *)

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
    | "!="      { NEQUAL }
    | '<'       { LT }
    | '>'       { GT }
    | "<="      { LEQ }
    | ">="      { GEQ }
    | '='       { ASSIGN }
    | "=="       { EQ }
    | ','       { COMMA }
    | ';'       { SEMICOLON }
    | '('       { LPAREN }
    | ')'       { RPAREN }
    | '['       { LBRACKETS }
    | ']'       { RBRACKETS }
    | '{'       { LBRACE }
    | '}'       { RBRACE }
    | '-'       { MINUS }
    | '+'       { PLUS }
    | '/'       { DIV }
    | '*'       { MULT }
    | "if"      { IF }
    | "else"    { ELSE }
    | "void"    { VOID }
    | "while"   { WHILE }
    | "return"  { RETURN }
    | '"'       { STRING (match_string (Buffer.create 100) lexbuf) }
    | "/*"      { match_comment lexbuf }
    | id        { ID (Lexing.lexeme lexbuf) }
    | number    { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | eof       { EOF }
    | _         { error lexbuf "The lexeme ('%s') is meaningless to C- language. (LEXEME ERROR)" (Lexing.lexeme lexbuf) }

and match_comment =
    parse
    | "*/"      { token lexbuf }
    | newline   { Lexing.new_line lexbuf; match_comment lexbuf }
    | eof       { error lexbuf "Comments must be closed. (COMMENT ERROR)" (Lexing.lexeme lexbuf) }
    | _         { match_comment lexbuf }

and match_string buf = 
    parse
    | [^'"' '\n' '\\']+  { Buffer.add_string buf (Lexing.lexeme lexbuf); match_string buf lexbuf }
    | '\n'               { Buffer.add_string buf (Lexing.lexeme lexbuf); Lexing.new_line lexbuf; match_string buf lexbuf }
    | '\\' '"'           { Buffer.add_char buf '"'; match_string buf lexbuf }
    | '\\'               { Buffer.add_char buf '\\'; match_string buf lexbuf }
    | '"'                { Buffer.contents buf }
    | eof                { error lexbuf "EOF was found inside a string. (STRING ERROR)" }
    | _                  { error lexbuf "The lexeme ('%s') does not make sense in this context. (STRING ERROR)" (Lexing.lexeme lexbuf) }

{
    
}
