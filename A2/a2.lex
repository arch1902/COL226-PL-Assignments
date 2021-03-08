(* ML-Lex user declarations *)

structure Tokens= Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token  
type lexresult = (svalue, pos) token
val output=[];



val pos = ref 0
val eof = fn () => Tokens.EOF(!pos, !pos)

val error : string * int * int -> unit = fn
    (e,l1,l2) => TextIO.output(TextIO.stdOut,"Unknown Token:"
                ^Int.toString l1^":"^Int.toString l2
                ^":"^e^"\n")


%%

%header (functor a2LexFun(structure Tokens: a2_TOKENS));


alpha =[A-Za-z];
ws = [\ \t];

%%

\n          => (print("\n");pos := (!pos) + 1; lex());
{ws}+       => (lex());



";"         => (print("TERM "^"\";\",\n");Tokens.TERM(!pos,!pos));


"("         => (print("LPAREN "^"\"(\", ");Tokens.LPAREN(!pos,!pos));
")"         => (print("RPAREN "^"\"(\", ");Tokens.RPAREN(!pos,!pos));
{alpha}+    => (   
                if yytext="TRUE" then (print("CONST "^"\"TRUE\", ");Tokens.CONST(yytext,!pos,!pos))
                else if yytext="FALSE" then (print("CONST "^"\"FALSE\", ");Tokens.CONST(yytext,!pos,!pos))
                else if yytext="NOT" then (print("NOT "^"\"NOT\", ");Tokens.NOT(yytext,!pos,!pos))
                else if yytext="AND" then (print("AND "^"\"AND\", ");Tokens.AND(yytext,!pos,!pos))
                else if yytext="OR" then (print("OR "^"\"OR\", ");Tokens.OR(yytext,!pos,!pos))
                else if yytext="XOR" then (print("XOR "^"\"XOR\", ");Tokens.XOR(yytext,!pos,!pos))
                else if yytext="EQUALS" then (print("EQUALS "^"\"EQUALS\", ");Tokens.EQUALS(yytext,!pos,!pos))
                else if yytext="IMPLIES" then (print("IMPLIES "^"\"IMPLIES\", ");Tokens.IMPLIES(yytext,!pos,!pos))
                else if yytext="IF" then (print("IF "^"\"IF\", ");Tokens.IF(yytext,!pos,!pos))
                else if yytext="THEN" then (print("THEN "^"\"THEN\", ");Tokens.THEN(yytext,!pos,!pos))
                else if yytext="ELSE" then (print("ELSE "^"\"ELSE\", ");Tokens.ELSE(yytext,!pos,!pos))
                else (print("ID "^"\""^yytext^"\", ");Tokens.ID(yytext,!pos,!pos))
            );

.           => (error(yytext,!pos,!pos);lex());



