(* User  declarations *)


%%
(* required declarations *)
%name a2

%term
  ID of string | CONST of string | NOT of string | AND of string | OR of string
| XOR of string | EQUALS of string | IMPLIES of string | IF of string | THEN of string | ELSE of string
| TERM  | RPAREN | LPAREN | EOF 

%nonterm program of string | statement of string | formula of string | C of string | START of string

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

  START : program (program^"START -> program")

  program : statement program (statement^program^"program -> statement program") |  EOF ("")

  statement :  formula TERM (formula1^"statement -> formula "^"TERM \";\" ") 

  formula : LPAREN formula RPAREN ("LPAREN \"(\" "^formula1^"RPAREN \")\" ")
  | IF formula THEN formula ELSE formula ("IF \"IF\" "^formula1^"EQUALS \"EQUALS\" "^formula2^"ELSE \"ELSE\" "^formula3)
  | formula AND formula (formula1^"AND \"AND\" "^formula2)
  | formula OR formula  (formula1^"OR \"OR\" "^formula2)
  | formula XOR formula  (formula1^"XOR \"XOR\" "^formula2)
  | formula EQUALS formula  (formula1^"EQUALS \"EQUALS\" "^formula2)
  | formula IMPLIES formula  (formula1^"IMPLIES \"(\" "^formula2)
  | C (C^"formula -> C")

  C: ID (ID1^"  ")
  | CONST (CONST1^)
  | NOT C ("C -> NOT C")


  

