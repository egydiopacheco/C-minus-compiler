type typ =
    | TInt
    | TVoid

type program = decl list
and decl =
    | SetVar of typ * string
    | SetArr of typ * string * int
    | SetFunc of typ * string * param list * var_decl list * stmt list

and var_decl =
    | VarSpecs of typ * string
    | ArrSpecs of typ * string * int

and param = 
    | VarParams of typ * string
    | ArrParams of typ * string

and stmt =
    | ExprDecl of expr option
    | ScopeDecl of var_decl list * stmt list
    | CondDecl of expr * stmt * stmt option
    | WhileDecl of expr * stmt
    | RetDecl of expr option

and expr = 
    | AssignExpr of string * expr
    | AssignArr of string * expr * expr
    | AssignMath of math_expr
    | AssignLogic of relop_expr

and math_expr =
    | Add of math_expr * math_expr
    | Sub of math_expr * math_expr
    | Mult of math_expr * math_expr
    | Div of math_expr * math_expr
    | Int of int
    | Var of string
    | Arr of string * expr
    | Expr of expr
    | Call of string * expr list

and relop_expr =
    | Leq of math_expr * math_expr
    | Lss of math_expr * math_expr
    | Gtr of math_expr * math_expr
    | Gte of math_expr * math_expr
    | Eq of math_expr * math_expr
    | Neq of math_expr * math_expr
