structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"

fun evalExp(e:exp, env:environment):value =
    case e of
	NumExp i            => IntVal i
      | StringExp s         => StringVal s
      | VarExp x            => envLookup (x, env) 				  
      | BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
      | UnExp(b,e1)         => 
            (
                case (b,evalExp(e1, env)) of
                    (Not,BoolVal b1)    => BoolVal(not b1)
                  | (Negate, IntVal i1) => IntVal(~i1)
            )

      | LetExp(ValDecl(x, e1), e2)  =>
            let
                val v1 = evalExp(e1, env)
            in
                evalExp(e2, envAdd (x, v1, env))
            end	
      | BoolExp b => BoolVal b
      | CondExp(e1,e2,e3) =>
            (case evalExp(e1,env) of
                 BoolVal b  => if b then evalExp(e2,env) else evalExp(e3,env) 
                        | _ => raise brokenTypes )	
    
      | FnExp(x1,_,_,e1) => FnVal ("",x1,e1,env)
      | AppExp(e1,e2) =>  
            (case evalExp(e1,env) of 
                FnVal ("",x1,body,enclosingEnv) =>
                (
                    let val v1 = evalExp(e2,env)
                    in
                        evalExp(body,envAdd(x1,v1,enclosingEnv))
                    end
                ) 
              | _ => raise brokenTypes

            ) 
      | FunExp(fncname,x1,_,_,e1) => FnVal(fncname,x1,e1,env)
            (* let val initialEnv = ref []
                val enclosingEnv = envAdd(fncname,FnVal(x1,body,initialEnv),env)
                val () = initialEnv :=  enclosingEnv *)

and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
    case (b, evalExp(e1, env), evalExp(e2, env))  of
        (Add, IntVal i1, IntVal i2) => IntVal (i1+i2)
    |   (Sub, IntVal i1, IntVal i2) => IntVal (i1-i2)
    |   (Mul, IntVal i1, IntVal i2) => IntVal (i1*i2)
    |   (Equals, IntVal i1, IntVal i2)  => BoolVal (i1 = i2)
    |   (Equals, StringVal s1, StringVal s2) => BoolVal (s1 = s2)
    |   (And, BoolVal s1, BoolVal s2) => BoolVal (s1 andalso s2)
    |   (Or, BoolVal s1, BoolVal s2) => BoolVal (s1 orelse s2)
    |   (Xor, BoolVal s1, BoolVal s2) => BoolVal ((s1 andalso not s2) orelse (not s1 andalso s2))
    |   (Implies, BoolVal s1, BoolVal s2) => BoolVal ((not s1) orelse s2)
    |   (Lessthan, IntVal s1, IntVal s2) => BoolVal (s1 < s2)
    |   (Greaterthan, IntVal s1, IntVal s2) => BoolVal (s1 > s2)
    |   _  => raise brokenTypes  					    
end
