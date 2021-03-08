functor a2LrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : a2_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* User  declarations *)



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\012\000\002\000\011\000\003\000\010\000\000\000\
\\001\000\001\000\012\000\002\000\011\000\003\000\010\000\009\000\009\000\
\\014\000\008\000\000\000\
\\001\000\001\000\012\000\002\000\011\000\003\000\010\000\009\000\009\000\
\\014\000\008\000\015\000\007\000\000\000\
\\001\000\004\000\018\000\005\000\017\000\006\000\016\000\007\000\015\000\
\\008\000\014\000\010\000\029\000\000\000\
\\001\000\004\000\018\000\005\000\017\000\006\000\016\000\007\000\015\000\
\\008\000\014\000\011\000\031\000\000\000\
\\001\000\004\000\018\000\005\000\017\000\006\000\016\000\007\000\015\000\
\\008\000\014\000\012\000\013\000\000\000\
\\001\000\004\000\018\000\005\000\017\000\006\000\016\000\007\000\015\000\
\\008\000\014\000\013\000\028\000\000\000\
\\001\000\012\000\000\000\015\000\000\000\000\000\
\\034\000\000\000\
\\035\000\000\000\
\\036\000\000\000\
\\037\000\000\000\
\\038\000\000\000\
\\039\000\004\000\018\000\005\000\017\000\006\000\016\000\007\000\015\000\
\\008\000\014\000\000\000\
\\040\000\005\000\017\000\006\000\016\000\007\000\015\000\000\000\
\\041\000\006\000\016\000\007\000\015\000\000\000\
\\042\000\007\000\015\000\000\000\
\\043\000\000\000\
\\044\000\004\000\018\000\005\000\017\000\006\000\016\000\007\000\015\000\
\\008\000\014\000\000\000\
\\045\000\000\000\
\\046\000\000\000\
\\047\000\000\000\
\\048\000\000\000\
\"
val actionRowNumbers =
"\002\000\019\000\005\000\002\000\
\\008\000\010\000\001\000\001\000\
\\000\000\021\000\020\000\011\000\
\\001\000\001\000\001\000\001\000\
\\001\000\009\000\006\000\003\000\
\\022\000\018\000\017\000\016\000\
\\015\000\014\000\012\000\001\000\
\\004\000\001\000\013\000\007\000"
val gotoT =
"\
\\001\000\004\000\002\000\003\000\003\000\002\000\004\000\001\000\
\\005\000\031\000\000\000\
\\000\000\
\\000\000\
\\001\000\017\000\002\000\003\000\003\000\002\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\\003\000\018\000\004\000\001\000\000\000\
\\003\000\019\000\004\000\001\000\000\000\
\\004\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\021\000\004\000\001\000\000\000\
\\003\000\022\000\004\000\001\000\000\000\
\\003\000\023\000\004\000\001\000\000\000\
\\003\000\024\000\004\000\001\000\000\000\
\\003\000\025\000\004\000\001\000\000\000\
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
\\003\000\028\000\004\000\001\000\000\000\
\\000\000\
\\003\000\030\000\004\000\001\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 32
val numrules = 15
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
datatype svalue = VOID | ntVOID of unit ->  unit
 | ELSE of unit ->  (string) | THEN of unit ->  (string)
 | IF of unit ->  (string) | IMPLIES of unit ->  (string)
 | EQUALS of unit ->  (string) | XOR of unit ->  (string)
 | OR of unit ->  (string) | AND of unit ->  (string)
 | NOT of unit ->  (string) | CONST of unit ->  (string)
 | ID of unit ->  (string) | START of unit ->  (string)
 | C of unit ->  (string) | formula of unit ->  (string)
 | statement of unit ->  (string) | program of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = string
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
fn (T 14) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "CONST"
  | (T 2) => "NOT"
  | (T 3) => "AND"
  | (T 4) => "OR"
  | (T 5) => "XOR"
  | (T 6) => "EQUALS"
  | (T 7) => "IMPLIES"
  | (T 8) => "IF"
  | (T 9) => "THEN"
  | (T 10) => "ELSE"
  | (T 11) => "TERM"
  | (T 12) => "RPAREN"
  | (T 13) => "LPAREN"
  | (T 14) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.program program1, program1left, 
program1right)) :: rest671)) => let val  result = MlyValue.START (fn _
 => let val  (program as program1) = program1 ()
 in (program^"START -> program")
end)
 in ( LrTable.NT 4, ( result, program1left, program1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.program program1, _, program1right)) :: ( _,
 ( MlyValue.statement statement1, statement1left, _)) :: rest671)) =>
 let val  result = MlyValue.program (fn _ => let val  (statement as 
statement1) = statement1 ()
 val  (program as program1) = program1 ()
 in (statement^program^"program -> statement program")
end)
 in ( LrTable.NT 0, ( result, statement1left, program1right), rest671)

end
|  ( 2, ( ( _, ( _, EOF1left, EOF1right)) :: rest671)) => let val  
result = MlyValue.program (fn _ => (""))
 in ( LrTable.NT 0, ( result, EOF1left, EOF1right), rest671)
end
|  ( 3, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.formula 
formula1, formula1left, _)) :: rest671)) => let val  result = 
MlyValue.statement (fn _ => let val  (formula as formula1) = formula1
 ()
 in (formula1^"statement -> formula "^"TERM \";\" ")
end)
 in ( LrTable.NT 1, ( result, formula1left, TERM1right), rest671)
end
|  ( 4, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.formula (fn _ => let val  formula1 = formula1
 ()
 in ("LPAREN \"(\" "^formula1^"RPAREN \")\" ")
end)
 in ( LrTable.NT 2, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.formula formula3, _, formula3right)) :: ( _,
 ( MlyValue.ELSE ELSE1, _, _)) :: ( _, ( MlyValue.formula formula2, _,
 _)) :: ( _, ( MlyValue.THEN THEN1, _, _)) :: ( _, ( MlyValue.formula 
formula1, _, _)) :: ( _, ( MlyValue.IF IF1, IF1left, _)) :: rest671))
 => let val  result = MlyValue.formula (fn _ => let val  (IF as IF1) =
 IF1 ()
 val  formula1 = formula1 ()
 val  THEN1 = THEN1 ()
 val  formula2 = formula2 ()
 val  (ELSE as ELSE1) = ELSE1 ()
 val  formula3 = formula3 ()
 in (
"IF \"IF\" "^formula1^"EQUALS \"EQUALS\" "^formula2^"ELSE \"ELSE\" "^formula3
)
end)
 in ( LrTable.NT 2, ( result, IF1left, formula3right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: ( _,
 ( MlyValue.AND AND1, _, _)) :: ( _, ( MlyValue.formula formula1, 
formula1left, _)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  formula1 = formula1 ()
 val  (AND as AND1) = AND1 ()
 val  formula2 = formula2 ()
 in (formula1^"AND \"AND\" "^formula2)
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: ( _,
 ( MlyValue.OR OR1, _, _)) :: ( _, ( MlyValue.formula formula1, 
formula1left, _)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  formula1 = formula1 ()
 val  (OR as OR1) = OR1 ()
 val  formula2 = formula2 ()
 in (formula1^"OR \"OR\" "^formula2)
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 8, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: ( _,
 ( MlyValue.XOR XOR1, _, _)) :: ( _, ( MlyValue.formula formula1, 
formula1left, _)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  formula1 = formula1 ()
 val  (XOR as XOR1) = XOR1 ()
 val  formula2 = formula2 ()
 in (formula1^"XOR \"XOR\" "^formula2)
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: ( _,
 ( MlyValue.EQUALS EQUALS1, _, _)) :: ( _, ( MlyValue.formula formula1
, formula1left, _)) :: rest671)) => let val  result = MlyValue.formula
 (fn _ => let val  formula1 = formula1 ()
 val  (EQUALS as EQUALS1) = EQUALS1 ()
 val  formula2 = formula2 ()
 in (formula1^"EQUALS \"EQUALS\" "^formula2)
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.formula formula2, _, formula2right)) :: ( _
, ( MlyValue.IMPLIES IMPLIES1, _, _)) :: ( _, ( MlyValue.formula 
formula1, formula1left, _)) :: rest671)) => let val  result = 
MlyValue.formula (fn _ => let val  formula1 = formula1 ()
 val  (IMPLIES as IMPLIES1) = IMPLIES1 ()
 val  formula2 = formula2 ()
 in (formula1^"IMPLIES \"(\" "^formula2)
end)
 in ( LrTable.NT 2, ( result, formula1left, formula2right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.C C1, C1left, C1right)) :: rest671)) => let
 val  result = MlyValue.formula (fn _ => let val  (C as C1) = C1 ()
 in (C^"formula -> C")
end)
 in ( LrTable.NT 2, ( result, C1left, C1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.C (fn _ => let val  ID1 = ID1 ()
 in (ID1)
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.CONST CONST1, CONST1left, CONST1right)) :: 
rest671)) => let val  result = MlyValue.C (fn _ => let val  CONST1 = 
CONST1 ()
 in (CONST1)
end)
 in ( LrTable.NT 3, ( result, CONST1left, CONST1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.C C1, _, C1right)) :: ( _, ( MlyValue.NOT 
NOT1, NOT1left, _)) :: rest671)) => let val  result = MlyValue.C (fn _
 => let val  (NOT as NOT1) = NOT1 ()
 val  (C as C1) = C1 ()
 in ("C -> NOT C")
end)
 in ( LrTable.NT 3, ( result, NOT1left, C1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : a2_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun CONST (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.CONST (fn () => i),p1,p2))
fun NOT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.NOT (fn () => i),p1,p2))
fun AND (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.AND (fn () => i),p1,p2))
fun OR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.OR (fn () => i),p1,p2))
fun XOR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.XOR (fn () => i),p1,p2))
fun EQUALS (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.EQUALS (fn () => i),p1,p2))
fun IMPLIES (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.IMPLIES (fn () => i),p1,p2))
fun IF (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.IF (fn () => i),p1,p2))
fun THEN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.THEN (fn () => i),p1,p2))
fun ELSE (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.ELSE (fn () => i),p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
end
end
