## To compile the lexer and parser
>ml-lex a3.lex
>ml-yacc a3.yacc

## Using INTERACTIVE SML in TERMINAL
>sml
>use "loader.sml";

## To build the AST
>parseString "test1.txt";

## To evaluate the expression
>open EVALUATOR;
>parseString "test1.txt";
>evalExp(it,[]);

## For Type-Checking
>open Typing;
>parseString "test1.txt";
>getType(it,[]);

