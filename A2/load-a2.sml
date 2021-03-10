val args = CommandLine.arguments()

structure a2LrVals = a2LrValsFun(structure Token = LrParser.Token)
structure a2Lex = a2LexFun(structure Tokens = a2LrVals.Tokens);
structure a2Parser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = a2LrVals.ParserData
     	       structure Lex = a2Lex)
     
fun invoke lexstream =
    	let fun print_error (s,pos:int,col:int) =
		    TextIO.output(TextIO.stdOut,s^" : "^ (Int.toString pos)^" : "^(Int.toString col)^ "\n")
		in
		    a2Parser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  a2Parser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = a2LrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
		val (nextToken, lexer) = a2Parser.Stream.get lexer
    in
        if a2Parser.sameToken(nextToken, dummyEOF) then result
 		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer



fun parseFile (infile:string) =
   let 
    	val instream = TextIO.openIn infile
	fun loop (instream,input) =
		case TextIO.inputLine instream of
	             SOME line =>  loop (instream,input^line)
    	    	   | NONE      => print(parseString(input))
				   					
    in
	 loop (instream,"") before TextIO.closeIn instream
    end;

parseFile(hd(args));

