structure AST =
struct

type id = string

datatype binop = Add | Sub | Mul | And | Or | Xor | Equals | Implies
				| Lessthan | Greaterthan

and unaryop = Negate | Not

and decl = ValDecl of id * exp

and typ = IntTy
			|  BoolTy
			|  StrTy
			|  FnTy of typ*typ
			|  TupTy of typ list

and exp = NumExp of int
    	| StringExp of string
    	| VarExp of id
		| BinExp of binop * exp * exp
		| LetExp of decl * exp
        | BoolExp of bool
		| CondExp of exp * exp * exp
		| UnExp of unaryop * exp
		| AppExp of exp * exp
		| FnExp of id * typ * typ * exp
		| TupExp of exp list
		| FunExp of id * id * typ * typ * exp

				       
datatype value = IntVal of int
               	| StringVal of string
	       		| BoolVal of bool
				| FnVal of id * id * exp * (id * value) list
				
type environment = (id * value) list

fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"							    
end


