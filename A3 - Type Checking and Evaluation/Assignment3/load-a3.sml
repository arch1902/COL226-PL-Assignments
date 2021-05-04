val args = CommandLine.arguments()

structure a3LrVals = a3LrValsFun(structure Token = LrParser.Token)
structure a3Lex = a3LexFun(structure Tokens = a3LrVals.Tokens);
structure a3Parser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = a3LrVals.ParserData
     	       structure Lex = a3Lex)
     
fun invoke lexstream =
    	let fun print_error (s,pos:int,col:int) =
		    TextIO.output(TextIO.stdOut,s^" : "^ (Int.toString pos)^" : "^(Int.toString col)^ "\n")
		in
		    a3Parser.parse(0,lexstream,print_error,())
		end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  a3Parser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = a3LrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
		val (nextToken, lexer) = a3Parser.Stream.get lexer
    in
        if a3Parser.sameToken(nextToken, dummyEOF) then result
 		else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer



fun parseFile (infile:string) =
   let 
    	val instream = TextIO.openIn infile
	fun loop (instream,input) =
		case TextIO.inputLine instream of
	             SOME line =>  loop (instream,input^line)
    	    	   | NONE      => parseString(input)
				   					
    in
	 loop (instream,"") before TextIO.closeIn instream
    end;



