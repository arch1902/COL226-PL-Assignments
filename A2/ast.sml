(* ast.sml *)
structure AST =
struct
datatype exp = form of string
| Id of string
| Unary of unop * exp
| BinExp of binop * exp * exp
| TerExp of ternop * exp * exp * exp 
and binop = AND | OR | XOR | EQUALS | IMPLIES 
and unop = NOT
and ternop = IFTHENELSE
end