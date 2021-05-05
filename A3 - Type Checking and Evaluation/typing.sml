structure Typing =
struct 

open AST 

type typEnv = (id * typ) list

fun typEnvLookup (var: id, env:typEnv): typ = 
    case List.find(fn(x,_) => x = var) env of
        SOME (x,v) => v
    |   NONE => raise Fail ("Variable " ^ var^" is w/o a type")

fun typEnvAdd(var:id , t:typ, env:typEnv):typEnv =
    (var,t)::env

fun getType(e:exp, env:typEnv):typ = 
    case e of 
        NumExp _ => IntTy
      | StringExp _ => StrTy
      | VarExp x => typEnvLookup(x,env)
      | BoolExp _ => BoolTy
      | AppExp(e1,e2) =>
        (case (getType(e1,env),getType(e2,env)) of
            (FnTy(t1,t2),t3) => 
            if(t1 = t3) 
            then t2
            else raise Fail "Application arguement type mismatch"
          | (_,_) => raise Fail "Function was expected" 
        )
      | FnExp (x,t1,t2,e) => 
            FnTy (t1, getType(e, typEnvAdd(x,t1,env)))

      | CondExp (e1,e2,e3) =>
        (
            let
                val t1 = getType(e1,env)
                val t2 = getType(e2,env)
                val t3 = getType(e3,env) 
            in
                if t1 <> BoolTy
                then raise Fail "Condition of If command is not of BoolTy"
                else 
                    if t2 <> t3
                    then raise Fail "Type Error:Branches have different types"
                    else t2
            end
        )
      | FunExp(fname,x1,t1,t2,e) =>
        (
            let
                val eTyp = getType(e,typEnvAdd(fname,t2,typEnvAdd(x1,t1,env)))
            in
                if eTyp <> t2 
                then raise Fail "Mismatch in declared type and actual type"
                else t2
            end
        )
    end