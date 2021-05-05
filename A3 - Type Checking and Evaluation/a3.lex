structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val pos = ref 0
  val eof = fn () => Tokens.EOF(!pos, !pos)
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut,"line " ^ (Int.toString l) ^ ": " ^ e ^ "\n")

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))


  val keywords =
  [
   ("end",  Tokens.END),
   ("in",  Tokens.IN),
   ("let",  Tokens.LET),
   ("PLUS",  Tokens.PLUS),
   ("MINUS",  Tokens.MINUS),
   ("TIMES",  Tokens.TIMES),
   ("NEGATE",  Tokens.NEGATE),
   ("EQUALS",  Tokens.EQUALS),   
   ("LESSTHAN",  Tokens.LESSTHAN),
   ("GREATERTHAN",  Tokens.GREATERTHAN),
   ("NOT",  Tokens.NOT),
   ("AND",  Tokens.AND),
   ("OR",  Tokens.OR),  
   ("XOR",  Tokens.XOR),
   ("EQUALS",  Tokens.EQUALS),
   ("IMPLIES",  Tokens.IMPLIES),
   ("if",  Tokens.IF),
   ("then",  Tokens.THEN),
   ("else",  Tokens.ELSE),
   ("fi",  Tokens.FI) ,
   ("fn",  Tokens.FN) ,
   ("fun",  Tokens.FUN) ,
   ("int",  Tokens.INT) ,
   ("bool",  Tokens.BOOL)    
   ]

  fun findKeywords (str:string, pos1:pos, pos2:pos) =
  case List.find (fn (s, _) => s = str )  keywords of 
  SOME (_, tk) => tk(pos1, pos2) 
  | NONE => Tokens.ID (str, pos1, pos2)
  %%
%header (functor a3LexFun(structure Tokens:a3_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];

%%

\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
";"         => (Tokens.TERM(!pos,!pos));

{digit}+ => (Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !pos, !pos));
"("      => (Tokens.LPAREN(!pos,!pos));
")"      => (Tokens.RPAREN(!pos,!pos));
"TRUE"   => (Tokens.CONST(true,!pos,!pos));
"FALSE"  => (Tokens.CONST(false,!pos,!pos));
{alpha}+ => (findKeywords(yytext,!pos,!pos));
"->"     => (Tokens.ARROW(!pos,!pos));
"=>"     => (Tokens.DARROW(!pos,!pos));
"="      => (Tokens.EQ(!pos,!pos)); 
":"      => (Tokens.COLON(!pos,!pos));
"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());

