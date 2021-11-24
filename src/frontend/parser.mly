%{
open Ast
exception Incorrect_Syntax
%}

%token <int> INT
%token <string> ID
%token ELSE IF RETURN VOID_TYPE INT_TYPE WHILE
%token PLUS MINUS TIMES DIV 
%token LT LEQ GT GEQ EQ NEQ
%token ASSIGN SEMI COMMA 
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token EOF


%start program
%type <Ast.program> program
%%

program:
dl = list_decl EOF { List.rev dl }

list_decl:
  | dl = list_decl; d = decl
    { d :: dl }
  | d = decl
    { [ d ] }

decl:
  | vd = var_decl
    {
      match vd with
      | VarSpecs(typespec, name) -> SetVar(typespec, name)
      | ArrSpecs(typespec, name, index) -> SetArr(typespec, name, index)
    }
  | fd = fun_decl
    { fd }

var_decl:
  | t = type_spec; i = ID; SEMI	
    { VarSpecs(t, i) }
  | t = type_spec; i = ID; LBRACKET; n = INT; RBRACKET; SEMI
    { ArrSpecs(t, i, n) }

type_spec: 
  | INT_TYPE	
    { TInt }
  | VOID_TYPE	
    { TVoid }

fun_decl:
  | t = type_spec; i =  ID; LPAREN; p = params; RPAREN; ss = stmt_scope
    {
      match ss with
      | (variables, statements) -> SetFunc(t, i, p, variables, statements)
    }

params:
  | pl = param_list
    { List.rev pl }
  | VOID_TYPE
    { [] }

param_list:
  | pl = param_list ; COMMA; p = param
    { p :: pl }
  | p = param
    { [ p ] }

param:
  | t = type_spec; i = ID 
    { VarParams(t, i) }
  | t = type_spec; i = ID; LBRACKET RBRACKET
    { ArrParams(t, i) }

stmt_scope:
  | LBRACE; ld = local_decl; sl = stmt_list; RBRACE	
    { (List.rev ld, List.rev sl) }

local_decl:
  | ld = local_decl; v = var_decl
    { v :: ld }
  | { [] }

stmt_list:
  | sl = stmt_list; st = stmt
    {
      match st with
      | ExprDecl None -> sl
      | _ -> st :: sl
    }
  | { [] }

stmt:
  | es = expr_stmt
    { es }
  | ss = stmt_scope
    {
      match ss with
      | (variables, statements) -> ScopeDecl(variables, statements)
    }
  | cs = cond_stmt
    { cs }
  | is = iter_stmt
    { is }
  | rs = return_stmt		
    { rs }

expr_stmt:
  | e = expr; SEMI
    { ExprDecl(Some e) }
  | SEMI 
    { ExprDecl(None) }

cond_stmt:
  | IF; LPAREN; e = expr; RPAREN; st = stmt
    { CondDecl(e, st, None) }
  | IF; LPAREN; e = expr; RPAREN; st = stmt; ELSE; st2 = stmt
    { CondDecl(e, st, Some st2) }

iter_stmt:
  | WHILE; LPAREN; e = expr; RPAREN; st = stmt
    { WhileDecl(e, st) }

return_stmt:
  | RETURN SEMI 
    { RetDecl (None) }
  | RETURN; e = expr; SEMI
    { RetDecl (Some e) }

expr:
  | v = var; ASSIGN; e = expr
    {
      match v with
      | (name, exprOption) -> match exprOption with
			      | None -> AssignExpr(name, e) 
			      | Some expr -> AssignArr(name, expr, e)
    }
  | se = simple_expr { se }

var:
  | i = ID
    { (i, None) }
  | i = ID; LBRACKET; e = expr; RBRACKET
    { (i, Some e) }

simple_expr:
  | ae1 = add_expr; re = relop; ae2 = add_expr
    {
      AssignLogic (
	  match re with
	  | LEQ -> Leq(ae1, ae2)
	  | LT -> Lss(ae1, ae2)
	  | GT -> Gtr(ae1, ae2)
	  | GEQ -> Gte(ae1, ae2)
	  | EQ -> Eq(ae1, ae2)
	  | NEQ -> Neq(ae1, ae2)
	  | _ -> raise Incorrect_Syntax
	)	
    }
  | ae = add_expr
    { AssignMath ae }

relop:
  | LEQ	{ LEQ }
  | LT	{ LT }
  | GT  { GT }
  | GEQ { GEQ }
  | EQ  { EQ }
  | NEQ { NEQ }

add_expr:
  | ae = add_expr; ao = add; t = term
    {						
      match ao with
      | PLUS -> Add(ae, t)
      | MINUS -> Sub(ae, t)
      | _ -> raise Incorrect_Syntax						
    }
  | t = term	
    { t }

add:
  | PLUS  { PLUS }
  | MINUS { MINUS }

term:
  | t = term; m = mult; f = factor
    {					
      match m with
      | TIMES -> Mult(t, f)
      | DIV -> Div(t, f)
      | _ -> raise Incorrect_Syntax	
    }
  | f = factor
    { f }

mult:
  | TIMES { TIMES }
  | DIV { DIV }

factor:
  | LPAREN; e = expr; RPAREN
    { Expr e }
  | v = var
    {								
      match v with
      | (name, option) -> 
	 match option with
	 | None -> Var name
	 | Some expr -> Arr(name, expr)
    }
  | c = call
    { c }
  | n = INT
    { Int n }

call:
  | i = ID; LPAREN; a = args; RPAREN
    { Call(i, a) }

args:
  | al = arg_list
    { List.rev al }
  |
    { [] }

arg_list:
  | al = arg_list; COMMA; e = expr
    { e :: al }
  | e = expr
    { [ e ] }

