(* ML-Lex user declarations *)

structure Tokens= Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token  
type lexresult = (svalue, pos) token
val output=[];



val pos = ref 1
val col = ref 1
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

\n          => (print("\n");pos := (!pos) + 1; col := 0;lex());
{ws}       => (col := (!col) + 1;lex());

";"         => (col := (!col) + 1;print("TERM "^"\";\",\n");Tokens.TERM(!pos,!pos));

"("         => (col := (!col) + 1;print("LPAREN "^"\"(\", ");Tokens.LPAREN(!pos,!pos));
")"         => (col := (!col) + 1;print("RPAREN "^"\"(\", ");Tokens.RPAREN(!pos,!pos));
{alpha}+    => (   
                if yytext="TRUE" then (col := (!col) + size(yytext);print("CONST "^"\"TRUE\", ");Tokens.CONST(yytext,!pos,!pos))
                else if yytext="FALSE" then (col := (!col) + size(yytext);print("CONST "^"\"FALSE\", ");Tokens.CONST(yytext,!pos,!pos))
                else if yytext="NOT" then (col := (!col) + size(yytext);print("NOT "^"\"NOT\", ");Tokens.NOT(yytext,!pos,!pos))
                else if yytext="AND" then (col := (!col) + size(yytext);print("AND "^"\"AND\", ");Tokens.AND(yytext,!pos,!pos))
                else if yytext="OR" then (col := (!col) + size(yytext);print("OR "^"\"OR\", ");Tokens.OR(yytext,!pos,!pos))
                else if yytext="XOR" then (col := (!col) + size(yytext);print("XOR "^"\"XOR\", ");Tokens.XOR(yytext,!pos,!pos))
                else if yytext="EQUALS" then (col := (!col) + size(yytext);print("EQUALS "^"\"EQUALS\", ");Tokens.EQUALS(yytext,!pos,!pos))
                else if yytext="IMPLIES" then (col := (!col) + size(yytext);print("IMPLIES "^"\"IMPLIES\", ");Tokens.IMPLIES(yytext,!pos,!pos))
                else if yytext="IF" then (col := (!col) + size(yytext);print("IF "^"\"IF\", ");Tokens.IF(yytext,!pos,!pos))
                else if yytext="THEN" then (col := (!col) + size(yytext);print("THEN "^"\"THEN\", ");Tokens.THEN(yytext,!pos,!pos))
                else if yytext="ELSE" then (col := (!col) + size(yytext);print("ELSE "^"\"ELSE\", ");Tokens.ELSE(yytext,!pos,!pos))
                else (col := (!col) + size(yytext);print("ID "^"\""^yytext^"\", ");Tokens.ID(yytext,!pos,!pos))
            );

.           => (error(yytext,!pos,!col);lex());



