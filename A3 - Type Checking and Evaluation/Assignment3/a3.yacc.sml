functor a3LrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : a3_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "a3.yacc"*)(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 


(*#line 16.1 "a3.yacc.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\013\000\002\000\012\000\003\000\023\000\004\000\022\000\
\\005\000\021\000\006\000\011\000\007\000\020\000\008\000\019\000\
\\010\000\048\000\011\000\010\000\014\000\009\000\018\000\008\000\
\\019\000\007\000\020\000\018\000\021\000\017\000\022\000\016\000\
\\023\000\015\000\024\000\014\000\025\000\006\000\030\000\005\000\
\\031\000\004\000\000\000\
\\001\000\001\000\013\000\002\000\012\000\006\000\011\000\011\000\010\000\
\\014\000\009\000\018\000\008\000\019\000\007\000\025\000\006\000\
\\030\000\005\000\031\000\004\000\000\000\
\\001\000\001\000\024\000\000\000\
\\001\000\001\000\029\000\000\000\
\\001\000\001\000\043\000\000\000\
\\001\000\001\000\049\000\000\000\
\\001\000\003\000\023\000\004\000\022\000\005\000\021\000\007\000\020\000\
\\008\000\019\000\010\000\054\000\020\000\018\000\021\000\017\000\
\\022\000\016\000\023\000\015\000\024\000\014\000\000\000\
\\001\000\003\000\023\000\004\000\022\000\005\000\021\000\007\000\020\000\
\\008\000\019\000\016\000\062\000\020\000\018\000\021\000\017\000\
\\022\000\016\000\023\000\015\000\024\000\014\000\000\000\
\\001\000\003\000\023\000\004\000\022\000\005\000\021\000\007\000\020\000\
\\008\000\019\000\020\000\018\000\021\000\017\000\022\000\016\000\
\\023\000\015\000\024\000\014\000\026\000\044\000\000\000\
\\001\000\003\000\023\000\004\000\022\000\005\000\021\000\007\000\020\000\
\\008\000\019\000\020\000\018\000\021\000\017\000\022\000\016\000\
\\023\000\015\000\024\000\014\000\027\000\061\000\000\000\
\\001\000\003\000\023\000\004\000\022\000\005\000\021\000\007\000\020\000\
\\008\000\019\000\020\000\018\000\021\000\017\000\022\000\016\000\
\\023\000\015\000\024\000\014\000\028\000\072\000\000\000\
\\001\000\010\000\065\000\034\000\064\000\000\000\
\\001\000\010\000\068\000\034\000\064\000\000\000\
\\001\000\010\000\071\000\034\000\064\000\000\000\
\\001\000\011\000\025\000\000\000\
\\001\000\011\000\042\000\000\000\
\\001\000\011\000\060\000\032\000\059\000\033\000\058\000\037\000\057\000\000\000\
\\001\000\012\000\000\000\000\000\
\\001\000\013\000\046\000\000\000\
\\001\000\015\000\045\000\000\000\
\\001\000\034\000\064\000\035\000\076\000\000\000\
\\001\000\034\000\064\000\035\000\077\000\000\000\
\\001\000\036\000\050\000\000\000\
\\001\000\036\000\055\000\000\000\
\\001\000\036\000\070\000\000\000\
\\001\000\036\000\073\000\000\000\
\\081\000\003\000\023\000\004\000\022\000\005\000\021\000\007\000\020\000\
\\008\000\019\000\020\000\018\000\021\000\017\000\022\000\016\000\
\\023\000\015\000\024\000\014\000\000\000\
\\082\000\003\000\023\000\004\000\022\000\005\000\021\000\007\000\020\000\
\\008\000\019\000\020\000\018\000\021\000\017\000\022\000\016\000\
\\023\000\015\000\024\000\014\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\004\000\022\000\007\000\020\000\008\000\019\000\020\000\018\000\
\\021\000\017\000\022\000\016\000\023\000\015\000\024\000\014\000\000\000\
\\086\000\004\000\022\000\007\000\020\000\008\000\019\000\020\000\018\000\
\\021\000\017\000\022\000\016\000\023\000\015\000\024\000\014\000\000\000\
\\087\000\007\000\020\000\008\000\019\000\020\000\018\000\021\000\017\000\
\\022\000\016\000\023\000\015\000\024\000\014\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\000\000\
\\094\000\007\000\020\000\008\000\019\000\020\000\018\000\021\000\017\000\
\\022\000\016\000\023\000\015\000\024\000\014\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\003\000\023\000\004\000\022\000\005\000\021\000\007\000\020\000\
\\008\000\019\000\020\000\018\000\021\000\017\000\022\000\016\000\
\\023\000\015\000\024\000\014\000\000\000\
\\102\000\003\000\023\000\004\000\022\000\005\000\021\000\007\000\020\000\
\\008\000\019\000\020\000\018\000\021\000\017\000\022\000\016\000\
\\023\000\015\000\024\000\014\000\000\000\
\\103\000\000\000\
\\104\000\034\000\064\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\"
val actionRowNumbers =
"\001\000\026\000\002\000\014\000\
\\001\000\001\000\045\000\003\000\
\\001\000\001\000\028\000\029\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\015\000\004\000\
\\008\000\041\000\019\000\018\000\
\\000\000\040\000\039\000\036\000\
\\035\000\034\000\033\000\038\000\
\\037\000\031\000\032\000\030\000\
\\005\000\022\000\001\000\001\000\
\\001\000\006\000\043\000\023\000\
\\016\000\009\000\007\000\027\000\
\\048\000\016\000\011\000\052\000\
\\051\000\050\000\016\000\001\000\
\\042\000\012\000\016\000\024\000\
\\013\000\010\000\025\000\049\000\
\\016\000\053\000\044\000\016\000\
\\020\000\021\000\001\000\001\000\
\\046\000\047\000\017\000"
val gotoT =
"\
\\001\000\001\000\002\000\078\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\024\000\000\000\
\\001\000\025\000\000\000\
\\000\000\
\\003\000\026\000\000\000\
\\001\000\028\000\000\000\
\\001\000\029\000\000\000\
\\000\000\
\\000\000\
\\001\000\030\000\000\000\
\\001\000\031\000\000\000\
\\001\000\032\000\000\000\
\\001\000\033\000\000\000\
\\001\000\034\000\000\000\
\\001\000\035\000\000\000\
\\001\000\036\000\000\000\
\\001\000\037\000\000\000\
\\001\000\038\000\000\000\
\\001\000\039\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\045\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\049\000\000\000\
\\001\000\050\000\000\000\
\\001\000\051\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\054\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\061\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\064\000\000\000\
\\001\000\065\000\000\000\
\\000\000\
\\000\000\
\\004\000\067\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\072\000\000\000\
\\000\000\
\\000\000\
\\004\000\073\000\000\000\
\\000\000\
\\000\000\
\\001\000\076\000\000\000\
\\001\000\077\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 79
val numrules = 28
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit | CONST of unit ->  (bool) | NUM of unit ->  (int) | ID of unit ->  (string) | Type of unit ->  (AST.typ) | DECL of unit ->  (AST.decl) | START of unit ->  (AST.exp) | EXP of unit ->  (AST.exp)
end
type svalue = MlyValue.svalue
type result = AST.exp
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 11) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "PLUS"
  | (T 3) => "TIMES"
  | (T 4) => "MINUS"
  | (T 5) => "NEGATE"
  | (T 6) => "LESSTHAN"
  | (T 7) => "GREATERTHAN"
  | (T 8) => "DIV"
  | (T 9) => "RPAREN"
  | (T 10) => "LPAREN"
  | (T 11) => "EOF"
  | (T 12) => "EQ"
  | (T 13) => "LET"
  | (T 14) => "IN"
  | (T 15) => "END"
  | (T 16) => "VAL"
  | (T 17) => "CONST"
  | (T 18) => "NOT"
  | (T 19) => "AND"
  | (T 20) => "OR"
  | (T 21) => "XOR"
  | (T 22) => "EQUALS"
  | (T 23) => "IMPLIES"
  | (T 24) => "IF"
  | (T 25) => "THEN"
  | (T 26) => "ELSE"
  | (T 27) => "FI"
  | (T 28) => "TERM"
  | (T 29) => "FN"
  | (T 30) => "FUN"
  | (T 31) => "INT"
  | (T 32) => "BOOL"
  | (T 33) => "ARROW"
  | (T 34) => "DARROW"
  | (T 35) => "COLON"
  | (T 36) => "STRING"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.START (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 47.14 "a3.yacc"*)EXP(*#line 342.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 1, ( result, EXP1left, EXP1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.DECL (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 49.20 "a3.yacc"*)AST.ValDecl(ID, EXP)(*#line 348.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, ID1left, EXP1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (NUM as NUM1) = NUM1 ()
 in ((*#line 51.13 "a3.yacc"*)AST.NumExp(NUM)(*#line 355.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, NUM1left, NUM1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 52.9 "a3.yacc"*)AST.VarExp(ID)(*#line 361.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 53.19 "a3.yacc"*)AST.BinExp(AST.Add, EXP1,  EXP2)(*#line 367.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 54.20 "a3.yacc"*)AST.BinExp(AST.Sub,  EXP1,  EXP2)(*#line 374.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 55.20 "a3.yacc"*)AST.BinExp(AST.Mul,  EXP1, EXP2)(*#line 381.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 56.18 "a3.yacc"*)AST.BinExp(AST.And,  EXP1, EXP2)(*#line 388.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 57.17 "a3.yacc"*)AST.BinExp(AST.Or,  EXP1, EXP2)(*#line 395.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 58.18 "a3.yacc"*)AST.BinExp(AST.Xor,  EXP1, EXP2)(*#line 402.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 59.21 "a3.yacc"*)AST.BinExp(AST.Equals,  EXP1, EXP2)(*#line 409.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 60.23 "a3.yacc"*)AST.BinExp(AST.Lessthan,  EXP1, EXP2)(*#line 416.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 61.26 "a3.yacc"*)AST.BinExp(AST.Greaterthan,  EXP1, EXP2)(*#line 423.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 62.22 "a3.yacc"*)AST.BinExp(AST.Implies,  EXP1, EXP2)(*#line 430.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, NEGATE1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 in ((*#line 63.16 "a3.yacc"*)AST.UnExp(AST.Negate,  EXP1)(*#line 437.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, NEGATE1left, EXP1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 in ((*#line 64.13 "a3.yacc"*)AST.UnExp(AST.Not,  EXP1)(*#line 443.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, NOT1left, EXP1right), rest671)
end
|  ( 16, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.DECL DECL1, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (DECL as DECL1) = DECL1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 65.26 "a3.yacc"*)AST.LetExp(DECL, EXP)(*#line 449.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 17, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 66.23 "a3.yacc"*)EXP(*#line 456.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 18, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.EXP EXP3, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP2, _, _)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 val  EXP3 = EXP3 ()
 in ((*#line 67.33 "a3.yacc"*)AST.CondExp(EXP1,EXP2,EXP3)(*#line 462.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, IF1left, FI1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (CONST as CONST1) = CONST1 ()
 in ((*#line 68.11 "a3.yacc"*)AST.BoolExp(CONST)(*#line 470.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, CONST1left, CONST1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( MlyValue.Type Type2, _, _)) :: _ :: _ :: ( _, ( MlyValue.Type Type1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( _, FN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 69.58 "a3.yacc"*)AST.FnExp(ID,Type1,Type2,EXP)(*#line 476.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, FN1left, EXP1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( MlyValue.Type Type2, _, _)) :: _ :: _ :: ( _, ( MlyValue.Type Type1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 70.62 "a3.yacc"*)AST.FunExp(ID1,ID2,Type1,Type2,EXP)(*#line 485.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, FUN1left, EXP1right), rest671)
end
|  ( 22, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP2, _, _)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 71.28 "a3.yacc"*)AST.AppExp(EXP1,EXP2)(*#line 495.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.Type Type2, _, Type2right)) :: _ :: ( _, ( MlyValue.Type Type1, Type1left, _)) :: rest671)) => let val  result = MlyValue.Type (fn _ => let val  Type1 = Type1 ()
 val  Type2 = Type2 ()
 in ((*#line 73.26 "a3.yacc"*)AST.FnTy(Type1,Type2)(*#line 502.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, Type1left, Type2right), rest671)
end
|  ( 24, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  result = MlyValue.Type (fn _ => ((*#line 74.26 "a3.yacc"*)AST.IntTy(*#line 509.1 "a3.yacc.sml"*)
))
 in ( LrTable.NT 3, ( result, INT1left, INT1right), rest671)
end
|  ( 25, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  result = MlyValue.Type (fn _ => ((*#line 75.26 "a3.yacc"*)AST.BoolTy(*#line 513.1 "a3.yacc.sml"*)
))
 in ( LrTable.NT 3, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 26, ( ( _, ( _, STRING1left, STRING1right)) :: rest671)) => let val  result = MlyValue.Type (fn _ => ((*#line 76.26 "a3.yacc"*)AST.StrTy(*#line 517.1 "a3.yacc.sml"*)
))
 in ( LrTable.NT 3, ( result, STRING1left, STRING1right), rest671)
end
|  ( 27, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Type Type1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.Type (fn _ => let val  (Type as Type1) = Type1 ()
 in ((*#line 77.26 "a3.yacc"*)Type(*#line 521.1 "a3.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : a3_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun NEGATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun LESSTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun GREATERTHAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.VOID,p1,p2))
fun FN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.VOID,p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID,p1,p2))
fun DARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(ParserData.MlyValue.VOID,p1,p2))
fun STRING (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(ParserData.MlyValue.VOID,p1,p2))
end
end
