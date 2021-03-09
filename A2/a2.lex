(* ML-Lex user declarations *)

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


%%


%header (functor a2LexFun(structure Tokens: a2_TOKENS));

alpha =[A-Za-z];
ws = [\ \t];



%%

\n          => (!out=(!out)^"\n";pos := (!pos) + 1; col := 1;lex());

{ws}        => (col := (!col) + 1;lex());

";"         => (col := (!col) + 1;out:=(!out)^"TERM "^"\";\", ";Tokens.TERM(!pos,!pos));

"("         => (col := (!col) + 1;out:=(!out)^"LPAREN "^"\"(\", ";Tokens.LPAREN(!pos,!pos));

")"         => (col := (!col) + 1;out:=(!out)^"RPAREN "^"\"(\", ";Tokens.RPAREN(!pos,!pos));

{alpha}+    => (   
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
            );

.           => (flag:= true;error(yytext,!pos,!col);col:=(!col)+1;lex());

