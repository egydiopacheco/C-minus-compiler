open Lexer

(* Create a hash table where both key and the content are strings. The hash table has a size of type int. *)
let create_hash (size: int) : (string, string) Hashtbl.t = Hashtbl.create size

(* Print to stdout the contents of a hash table where both key and content are strings *)
let print_hash (ht : (string, string) Hashtbl.t) = Hashtbl.iter (fun x y -> Printf.printf "{ %s : %s }\n" x y) ht

(* Function that receive a lexeme and a hash table. Pattern match the lexeme, and then add it to the hash table *)
let to_table x ht =
    match x with
    | PLUS              -> Hashtbl.add ht "+"               "| binop     | PLUS   |"
    | INT(d)            -> Hashtbl.add ht (string_of_int d) "| INT       |   -    |"
    | MINUS             -> Hashtbl.add ht "-"               "| binop     | MINUS  |"
    | DIV               -> Hashtbl.add ht "/"               "| binop     | DIV    |"
    | MULT              -> Hashtbl.add ht "*"               "| binop     | MULT   |"
    | ID(str)           -> Hashtbl.add ht str               "| ID        |   -    |"
    | LBRACKETS         -> Hashtbl.add ht "["               "| LBRACKETS |   -    |"
    | RBRACKETS         -> Hashtbl.add ht "]"               "| RBRACKETS |   -    |"
    | LBRACE            -> Hashtbl.add ht "{"               "| LBRACE    |   -    |"
    | RBRACE            -> Hashtbl.add ht "}"               "| RBRACE    |   -    |"
    | STRING(str)       -> Hashtbl.add ht str               "| STRING    |   -    |"
    | ASSIGN            -> Hashtbl.add ht "="               "| relop     | ASSIGN |"
    | LT                -> Hashtbl.add ht "<"               "| relop     | LT     |"
    | LEQ               -> Hashtbl.add ht "<="              "| relop     | LEQ    |"
    | GT                -> Hashtbl.add ht ">"               "| relop     | GT     |"
    | GEQ               -> Hashtbl.add ht ">="              "| relop     | GEQ    |"
    | NEQUAL            -> Hashtbl.add ht "!="              "| relop     | NEQUAL |"
    | EQ                -> Hashtbl.add ht "=="              "| relop     | EQ     |"
    | RETURN            -> Hashtbl.add ht "return"          "| RETURN    |   -    |"
    | WHILE             -> Hashtbl.add ht "while"           "| WHILE     |   -    |"
    | VOID              -> Hashtbl.add ht "void"            "| VOID      |   -    |"
    | IF                -> Hashtbl.add ht "if"              "| IF        |   -    |"
    | ELSE              -> Hashtbl.add ht "else"            "| ELSE      |   -    |"
    | COMMA             -> Hashtbl.add ht ","               "| COMMA     |   -    |"
    | SEMICOLON         -> Hashtbl.add ht ";"               "| SEMICOLON |   -    |"
    | LPAREN            -> Hashtbl.add ht "("               "| LPAREN    |   -    |"
    | RPAREN            -> Hashtbl.add ht ")"               "| RPAREN    |   -    |"
    | EOF               -> Hashtbl.add ht "eof"             "| EOF       |   -    |"
