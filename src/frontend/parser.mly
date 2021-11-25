%{
open Ast
%}

%token <int> INT
%token <string> ID
%token IF ELSE VOID_TYPE INT_TYPE WHILE RETURN
%token PLUS MINUS TIMES DIV
%token LT LEQ GT GEQ EQ NEQ
%token ASSIGN SEMI COMMA
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE
%token EOF
%token THEN

%right THEN ELSE

%start program
%type <Ast.program> program

%%

let program :=
dl = list_decl; EOF; { List.rev dl }

let list_decl :=
  | dl = list_decl; d = decl;
    { d :: dl }
  | d = decl;
    { [ d ] }

let decl :=
  | vd = var_decl;
    {
      match vd with
      | VarSpecs(typ, name) -> SetVar(typ, name)
      | ArrSpecs(typ, name, size) -> SetArr(typ, name, size)
    }
  | fd = fun_decl;
    { fd }

let var_decl :=
  | t = type_spec; i = ID; SEMI;
    { VarSpecs(t, i) }
  | t = type_spec; i = ID; LBRACKET; n = INT; RBRACKET; SEMI;
    { ArrSpecs(t, i, n) }

let type_spec := 
  | INT_TYPE;
    { TInt }
  | VOID_TYPE;	
    { TVoid }

let fun_decl :=
  | t = type_spec; i =  ID; LPAREN; p = params; RPAREN; ss = stmt_scope;
    {
      match ss with
      | (variables, statements) -> SetFunc(t, i, p, variables, statements)
    }

let params :=
  | pl = param_list;
    { List.rev pl }
  | VOID_TYPE;
    { [] }

let param_list :=
  | pl = param_list ; COMMA; p = param;
    { p :: pl }
  | p = param;
    { [ p ] }

let param :=
  | t = type_spec; i = ID ;
    { VarParams(t, i) }
  | t = type_spec; i = ID; LBRACKET; RBRACKET;
    { ArrParams(t, i) }

let stmt_scope :=
  | LBRACE; ld = local_decl; sl = stmt_list; RBRACE;
    { (List.rev ld, List.rev sl) }

let local_decl :=
  | ld = local_decl; v = var_decl;
    { v :: ld }
  | { [] }

let stmt_list :=
  | sl = stmt_list; st = stmt;
    {
      match st with
      | ExprDecl None -> sl
      | _ -> st :: sl
    }
  | { [] }

let stmt :=
  | es = expr_stmt;
    { es }
  | ss = stmt_scope;
    {
      match ss with
      | (variables, statements) -> ScopeDecl(variables, statements)
    }
  | cs = cond_stmt;
    { cs }
  | is = iter_stmt;
    { is }
  | rs = return_stmt;		
    { rs }

let expr_stmt :=
  | e = expr; SEMI;
    { ExprDecl(Some e) }
  | SEMI;
    { ExprDecl(None) }


let cond_stmt :=
  | IF; LPAREN; e = expr; RPAREN; st = stmt;
    { CondDecl(e, st, None) } %prec THEN
  | IF; LPAREN; e = expr; RPAREN; st = stmt; ELSE; st2 = stmt;
    { CondDecl(e, st, Some st2) }


let iter_stmt :=
  | WHILE; LPAREN; e = expr; RPAREN; st = stmt;
    { WhileDecl(e, st) }

let return_stmt :=
  | RETURN; SEMI;
    { RetDecl(None) }
  | RETURN; e = expr; SEMI;
    { RetDecl(Some e) }

let expr :=
  | v = var; ASSIGN; e = expr;
    {
      match v with
      | (name, option) -> match option with
			  | None -> AssignExpr(name, e) 
			  | Some expr -> AssignArr(name, expr, e)
    }
  | se = simple_expr;
    { se }

let var :=
  | i = ID;
    { (i, None) }
  | i = ID; LBRACKET; e = expr; RBRACKET;
    { (i, Some e) }

let simple_expr :=
  | ae1 = add_expr; re = relop; ae2 = add_expr;
    {
      AssignLogic (
	  match re with
	  | LEQ -> Leq(ae1, ae2)
	  | LT -> Lss(ae1, ae2)
	  | GT -> Gtr(ae1, ae2)
	  | GEQ -> Gte(ae1, ae2)
	  | EQ -> Eq(ae1, ae2)
	  | NEQ -> Neq(ae1, ae2)
	  | _ -> failwith "Implement error handling"
	)	
    }
  | ae = add_expr;
    { AssignMath ae }

let relop ==
  | LEQ; { LEQ }
  | LT;	 { LT }
  | GT;  { GT }
  | GEQ; { GEQ }
  | EQ;  { EQ }
  | NEQ; { NEQ }

let add_expr :=
  | ae = add_expr; ao = additive_operator; t = term;
    {						
      match ao with
      | PLUS -> Add(ae, t)
      | MINUS -> Sub(ae, t)
      | _ -> failwith "Implement error handling"				
    }
  | t = term;
    { t }

let additive_operator ==
  | PLUS;  { PLUS }
  | MINUS; { MINUS }

let term :=
  | t = term; m = multiplicative_operator; f = factor;
    {					
      match m with
      | TIMES -> Mult(t, f)
      | DIV -> Div(t, f)
      | _ -> failwith "Implement error handling"	
    }
  | f = factor;
    { f }

let multiplicative_operator ==
  | TIMES; { TIMES }
  | DIV; { DIV }

let factor :=
  | LPAREN; e = expr; RPAREN;
    { Expr e }
  | v = var;
    {								
      match v with
      | (name, option) -> 
	 match option with
	 | None -> Var name
	 | Some expr -> Arr(name, expr)
    }
  | c = call;
    { c }
  | n = INT;
    { Int n }

let call :=
  | i = ID; LPAREN; a = args; RPAREN;
    { Call(i, a) }

let args :=
  | al = arg_list;
    { List.rev al }
  |
    { [] }

let arg_list :=
  | al = arg_list; COMMA; e = expr;
    { e :: al }
  | e = expr;
    { [ e ] }

