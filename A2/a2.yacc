(* User  declarations *)


%%
(* required declarations *)
%name a2

%term
  ID of string | CONST of string | NOT of string | AND of string | OR of string
| XOR of string | EQUALS of string | IMPLIES of string | IF of string | THEN of string | ELSE of string
| TERM  | RPAREN | LPAREN | EOF 

%nonterm program of string | statement of string | formula of string | START of string

%pos int


(*optional declarations *)

%eop EOF TERM
%noshift EOF

(* %header  *)

%right IF THEN ELSE
%right IMPLIES 
%left AND
%left OR
%left XOR
%left EQUALS
%right NOT 



(* %nonassoc *)


%verbose
%start START

%%

  START : program (program^"START -> program\n")

  program : statement program (statement^"program -> statement,"^program^"program -> program, ") |  EOF ("")

  statement :  formula TERM (formula1^"statement -> formula, "^"TERM \";\",") 

  formula : LPAREN formula RPAREN ("LPAREN \"(\", "^formula1^"RPAREN \")\", ")
  | IF formula THEN formula ELSE formula ("IF \"IF\", "^formula1^"formula->formula,"^"THEN \"THEN\", "^formula2^"formula->formula,"^"ELSE \"ELSE\", "^formula3^"formula->formula,")
  | formula AND formula (formula1^"formula->formula,"^"AND "^"\""^"AND"^"\""^","^formula2^"formula->formula,")
  | formula OR formula  (formula1^"formula->formula,"^"OR \"OR\","^formula2^"formula->formula,")
  | formula XOR formula  (formula1^"formula->formula,"^"XOR \"XOR\","^formula2^"formula->formula,")
  | formula EQUALS formula  (formula1^"formula->formula,"^"EQUALS \"EQUALS\","^formula2^"formula->formula,")
  | formula IMPLIES formula  (formula1^"formula->formula,"^"IMPLIES \"IMPLIES\","^formula2^"formula->formula,")
  | NOT formula ("NOT "^"\""^"NOT"^"\","^formula^"formula->formula,")
  | ID ("ID "^"\""^ID1^"\""^",")
  | CONST ("CONST "^"\""^CONST1^"\""^",")




  
