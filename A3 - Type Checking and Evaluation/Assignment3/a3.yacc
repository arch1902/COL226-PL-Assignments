(* User  declarations *)
fun lookup "special" = 1000
  | lookup s = 0 

%%
(* required declarations *)
%name a3

%term
  ID of string | NUM of int
| PLUS | TIMES | MINUS | NEGATE | LESSTHAN | GREATERTHAN 
| DIV  | RPAREN | LPAREN | EOF
| EQ | LET | IN | END | VAL | CONST of bool | NOT | AND | OR
| XOR | EQUALS | IMPLIES | IF | THEN | ELSE | FI
| TERM  | FN | FUN | INT | BOOL | ARROW | DARROW | COLON | STRING

%nonterm EXP of AST.exp | START of AST.exp  
          | DECL of AST.decl  | Type of AST.typ
          

%pos int

(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%right ARROW
%nonassoc DARROW

%left EQ  
%left MINUS PLUS
%left TIMES 
(* %right *)
%start START

%right IF THEN ELSE
%right IMPLIES 
%left AND OR XOR EQUALS LESSTHAN GREATERTHAN
%right NOT NEGATE

%verbose

%%

  START: EXP(EXP)

  DECL: ID EQ EXP (AST.ValDecl(ID, EXP))

  EXP: NUM (AST.NumExp(NUM))
  | ID (AST.VarExp(ID))
  | EXP PLUS EXP (AST.BinExp(AST.Add, EXP1,  EXP2))
  | EXP MINUS EXP (AST.BinExp(AST.Sub,  EXP1,  EXP2))
  | EXP TIMES EXP (AST.BinExp(AST.Mul,  EXP1, EXP2))
  | EXP AND EXP (AST.BinExp(AST.And,  EXP1, EXP2))
  | EXP OR EXP (AST.BinExp(AST.Or,  EXP1, EXP2))
  | EXP XOR EXP (AST.BinExp(AST.Xor,  EXP1, EXP2)) 
  | EXP EQUALS EXP (AST.BinExp(AST.Equals,  EXP1, EXP2))
  | EXP LESSTHAN EXP (AST.BinExp(AST.Lessthan,  EXP1, EXP2))
  | EXP GREATERTHAN EXP (AST.BinExp(AST.Greaterthan,  EXP1, EXP2))
  | EXP IMPLIES EXP (AST.BinExp(AST.Implies,  EXP1, EXP2))
  | NEGATE EXP(AST.UnExp(AST.Negate,  EXP1))
  | NOT EXP(AST.UnExp(AST.Not,  EXP1))
  | LET DECL IN EXP END (AST.LetExp(DECL, EXP))
  | LPAREN EXP RPAREN(EXP)
  | IF EXP THEN EXP ELSE EXP FI(AST.CondExp(EXP1,EXP2,EXP3)) 
  | CONST(AST.BoolExp(CONST))
  | FN LPAREN ID COLON Type RPAREN COLON Type DARROW EXP(AST.FnExp(ID,Type1,Type2,EXP))
  | FUN ID LPAREN ID COLON Type RPAREN COLON Type DARROW EXP(AST.FunExp(ID1,ID2,Type1,Type2,EXP))
  | LPAREN EXP EXP RPAREN (AST.AppExp(EXP1,EXP2))

Type: Type ARROW Type   (AST.FnTy(Type1,Type2))
  | INT                 (AST.IntTy)
  | BOOL                (AST.BoolTy)
  | STRING              (AST.StrTy)
  | LPAREN Type RPAREN  (Type)
