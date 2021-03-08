datatype lexresult=   ID of string | CONST of string | NOT of string | AND of string | OR of string
| XOR of string | EQUALS of string | IMPLIES of string | IF of string | THEN of string | ELSE of string
| TERM of string | RPAREN of string | LPAREN of string | EOF of string
val linenum = ref 1
val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
val eof = fn () => EOF
fun refinc x =  (x := !x + 1; !x)
  
%%
  
%structure SimpLex
alpha=[A-Za-z];
ws = [\ \t \n];

%%x


\n => (refinc linenum; lex());
{ws}+       => (lex());
";"         => (TERM);
"("         => (LPAREN);
")"         => (RPAREN);
{alpha}+    => (   
                if yytext="TRUE" then CONST yytext
                else if yytext="FALSE" then CONST yytext
                else if yytext="NOT" then NOT yytext
                else if yytext="AND" then AND yytext
                else if yytext="OR" then OR yytext
                else if yytext="XOR" then XOR yytext
                else if yytext="EQUALS" then EQUALS yytext
                else if yytext="IMPLIES" then IMPLIES yytext
                else if yytext="IF" then IF yytext
                else if yytext="THEN" then THEN yytext
                else if yytext="ELSE" then ELSE yytext
                else (ID yytext);lex()
            );
. => (error ("calc: ignoring bad character "^yytext); lex());