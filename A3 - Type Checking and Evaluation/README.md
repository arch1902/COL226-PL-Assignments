# Type Checking and Evaluation

## To compile the lexer and parser
```ml-lex a3.lex```

```ml-yacc a3.yacc```

## Using Interactive SML in TERMINAL
```use "loader.sml";```

## To build the AST
```parseFile "test1.txt";```

## To evaluate the expression
```open EVALUATOR;```

```parseFile "test1.txt";```

```evalExp(it,[]);```

## For Type-Checking
```open Typing;```

```parseFile "test1.txt";```

```getType(it,[]);```

