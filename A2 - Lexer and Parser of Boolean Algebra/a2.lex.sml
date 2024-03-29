(*#line 28.10 "a2.lex"*)functor a2LexFun(structure Tokens: a2_TOKENS)(*#line 1.1 "a2.lex.sml"*)
=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "a2.lex"*)(* ML-Lex user declarations *)

structure Tokens= Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token  
type lexresult = (svalue, pos) token



val output=[];
val pos = ref 1
val col = ref 1
val flag = ref false 
val flag2 = ref false
val out = ref "["
val eof = fn () => (if !flag=true then OS.Process.exit(OS.Process.success) else if !flag2=false then print(substring((!out) ,0,size((!out))-2)^"]"^"\n\n") else print "";flag2:=true;Tokens.EOF(!pos, !pos))

val error : string * int * int -> unit = fn
    (e,l1,l2) => TextIO.output(TextIO.stdOut,"Unknown Token:"
                ^Int.toString l1^":"^Int.toString l2
                ^":"^e^"\n")


(*#line 30.1 "a2.lex.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\009\010\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\009\003\003\003\003\003\003\003\008\007\003\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\006\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003\004\004\004\004\004\004\004\004\004\004\004\004\004\004\004\
\\004\004\004\004\004\004\004\004\004\004\004\003\003\003\003\003\
\\003"
),
 (4, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = List.map f (List.rev (tl (List.rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(List.map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 14)], trans = 0},
{fin = [(N 12),(N 14)], trans = 4},
{fin = [(N 12)], trans = 4},
{fin = [(N 5),(N 14)], trans = 0},
{fin = [(N 9),(N 14)], trans = 0},
{fin = [(N 7),(N 14)], trans = 0},
{fin = [(N 3),(N 14)], trans = 0},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => ((*#line 37.17 "a2.lex"*)!out=(!out)^"\n";pos := (!pos) + 1; col := 1;lex()(*#line 135.1 "a2.lex.sml"*)
)
| 12 => let val yytext=yymktext() in (*#line 47.17 "a2.lex"*)   
                if yytext="TRUE" 
                    then (col := (!col) + size(yytext);out:=(!out)^"CONST "^"\"TRUE\", ";Tokens.CONST(yytext,!pos,!pos))
                else if yytext="FALSE" 
                    then (col := (!col) + size(yytext);out:=(!out)^"CONST "^"\"FALSE\", ";Tokens.CONST(yytext,!pos,!pos))
                else if yytext="NOT" 
                    then (col := (!col) + size(yytext);out:=(!out)^"NOT "^"\"NOT\", ";Tokens.NOT(yytext,!pos,!pos))
                else if yytext="AND" 
                    then (col := (!col) + size(yytext);out:=(!out)^"AND "^"\"AND\", ";Tokens.AND(yytext,!pos,!pos))
                else if yytext="OR" 
                    then (col := (!col) + size(yytext);out:=(!out)^"OR "^"\"OR\", ";Tokens.OR(yytext,!pos,!pos))
                else if yytext="XOR" 
                    then (col := (!col) + size(yytext);out:=(!out)^"XOR "^"\"XOR\", ";Tokens.XOR(yytext,!pos,!pos))
                else if yytext="EQUALS" 
                    then (col := (!col) + size(yytext);out:=(!out)^"EQUALS "^"\"EQUALS\", ";Tokens.EQUALS(yytext,!pos,!pos))
                else if yytext="IMPLIES" 
                    then (col := (!col) + size(yytext);out:=(!out)^"IMPLIES "^"\"IMPLIES\", ";Tokens.IMPLIES(yytext,!pos,!pos))
                else if yytext="IF" 
                    then (col := (!col) + size(yytext);out:=(!out)^"IF "^"\"IF\", ";Tokens.IF(yytext,!pos,!pos))
                else if yytext="THEN" 
                    then (col := (!col) + size(yytext);out:=(!out)^"THEN "^"\"THEN\", ";Tokens.THEN(yytext,!pos,!pos))
                else if yytext="ELSE" 
                    then (col := (!col) + size(yytext);out:=(!out)^"ELSE "^"\"ELSE\", ";Tokens.ELSE(yytext,!pos,!pos))
                else 
                    (col := (!col) + size(yytext);out:=(!out)^"ID "^"\""^yytext^"\", ";Tokens.ID(yytext,!pos,!pos))
            (*#line 162.1 "a2.lex.sml"*)
 end
| 14 => let val yytext=yymktext() in (*#line 74.17 "a2.lex"*)flag:= true;error(yytext,!pos,!col);col:=(!col)+1;lex()(*#line 164.1 "a2.lex.sml"*)
 end
| 3 => ((*#line 39.17 "a2.lex"*)col := (!col) + 1;lex()(*#line 166.1 "a2.lex.sml"*)
)
| 5 => ((*#line 41.17 "a2.lex"*)col := (!col) + 1;out:=(!out)^"TERM "^"\";\", ";Tokens.TERM(!pos,!pos)(*#line 168.1 "a2.lex.sml"*)
)
| 7 => ((*#line 43.17 "a2.lex"*)col := (!col) + 1;out:=(!out)^"LPAREN "^"\"(\", ";Tokens.LPAREN(!pos,!pos)(*#line 170.1 "a2.lex.sml"*)
)
| 9 => ((*#line 45.17 "a2.lex"*)col := (!col) + 1;out:=(!out)^"RPAREN "^"\"(\", ";Tokens.RPAREN(!pos,!pos)(*#line 172.1 "a2.lex.sml"*)
)
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
