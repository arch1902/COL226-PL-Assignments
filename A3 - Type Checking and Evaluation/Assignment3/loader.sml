CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "ast.sml";
use "typing.sml";
use "evaluator.sml";
use "a3.yacc.sig";
use "a3.yacc.sml";
use "a3.lex.sml";
use "load-a3.sml";
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)
