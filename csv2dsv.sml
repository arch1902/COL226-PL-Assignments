fun read_file (infile:string) =
   let 
    	val instream = TextIO.openIn infile
	fun loop instream =
		case TextIO.inputLine instream of
	             SOME line => line :: loop instream
    	    	   | NONE      => []
    in
	 loop instream before TextIO.closeIn instream
    end;


exception emptyInputFile;
exception UnevenFields of int*int*int;


fun convertDelimiters(infilename, delim1, outfilename, delim2) = 
  let
    val ins = TextIO.openIn infilename
    val outs = TextIO.openOut outfilename
    val input = read_file(infilename)
    

    fun helper(prev : char, ch : char option ,cnt : int,prevcnt :int,linenum :int, bt :int , field :string , size : int , dq :int) =
      case ch of
           NONE => 
                        (TextIO.closeIn ins; TextIO.closeOut outs)
                        
         | SOME(c) => 

            if (c,bt)= (#"\n",0) then 
                if linenum=1 then (
                          if dq=1 then (TextIO.output1(outs,#"\""); TextIO.output(outs,field);TextIO.output1(outs,#"\""))
                          else TextIO.output(outs,field);
                          TextIO.output1(outs,c) ;
                          helper(c,TextIO.input1 ins,1,cnt,linenum+1,0,"",0,0))
                else if cnt=prevcnt then (
                          if dq=1 then (TextIO.output1(outs,#"\""); TextIO.output(outs,field);TextIO.output1(outs,#"\""))
                          else TextIO.output(outs,field);
                          TextIO.output1(outs,c);
                          helper(c,TextIO.input1 ins,1,cnt,linenum+1,0,"",0,0))
                else raise UnevenFields(prevcnt,cnt,linenum)

            else if (c,bt)= (#"\n",1) then (helper(c,TextIO.input1 ins,cnt,prevcnt,linenum,bt,field^"\n",size+1,0))

            else if (c,bt) = (delim1,1) then ( helper(c,TextIO.input1 ins,cnt,prevcnt,linenum,bt,field^String.str(c),size+1,0))

            else if (c,bt) = (delim1,0) then (
                            if dq>0 then (TextIO.output1(outs,#"\""); TextIO.output(outs,field);TextIO.output1(outs,#"\""))
                            else TextIO.output(outs,field);
                            TextIO.output1(outs,delim2);
                            helper(c,TextIO.input1 ins,cnt+1,prevcnt,linenum,bt,"",0,0))

            else if c = delim2 then (helper(c,TextIO.input1 ins,cnt,prevcnt,linenum,bt,field^String.str(c),size+1,dq+1))

            else if c = #"\"" then  (helper(c,TextIO.input1 ins,cnt,prevcnt,linenum,1-bt,field^String.str(c),size+1,0))

            else (helper(c,TextIO.input1 ins,cnt,prevcnt,linenum,bt,field^String.str(c),size+1,dq))

  in
    if input = nil then raise emptyInputFile
    else helper(#"\n",TextIO.input1 ins,1,1,1,0,"",0,0)
  end
    handle UnevenFields(a,b,c) => print("Expected: "^Int.toString(a)^" fields, Present: "^Int.toString(b)^" fields on Line "^Int.toString(c)^"\n");



convertDelimiters("himym.csv",#",","t1.txt",#";");
convertDelimiters("himym.csv",#",","t2.txt",#"&");
convertDelimiters("himym.csv",#",","t3.txt",#"|");



fun csv2tsv(infilename, outfilename) =
    convertDelimiters(infilename,#",",outfilename,#"\t");

csv2tsv("himym.csv","t4.txt");


fun tsv2csv(infilename, outfilename) =
    convertDelimiters(infilename,#"\t",outfilename,#",");

tsv2csv("t4.txt","t5.txt");



fun convertNewlines(infilename, newline1, outfilename, newline2) =
  let
    val ins = TextIO.openIn infilename
    val outs = TextIO.openOut outfilename
    val input = read_file(infilename)
    

    fun helper(prev : char, ch : char option ,cnt : int,prevcnt :int,linenum :int, bt :int , field :string , size : int , dq :int, eol :string) =
      case ch of
           NONE => 
                        (TextIO.closeIn ins; TextIO.closeOut outs)
                        
         | SOME(c) => 

            if (eol^String.str(c),bt)= (newline1,0) then 
                if linenum=1 then (
                          TextIO.output(outs,field);
                          TextIO.output(outs,newline2);
                          helper(c,TextIO.input1 ins,1,cnt,linenum+1,0,"",0,0,""))
                else if cnt=prevcnt then (
                          TextIO.output(outs,field);
                          TextIO.output(outs,newline2);
                          helper(c,TextIO.input1 ins,1,cnt,linenum+1,0,"",0,0,""))
                else raise UnevenFields(prevcnt,cnt,linenum)

            else if (c,bt)= (#"\n",0) then (helper(c,TextIO.input1 ins,cnt,prevcnt,linenum,bt,field,size+1,0,eol^String.str(c)))

            else if (c,bt)= (#"\r",0) then (helper(c,TextIO.input1 ins,cnt,prevcnt,linenum,bt,field,size+1,0,eol^String.str(c)))

            else if (c,bt)= (#"\n",1) then (helper(c,TextIO.input1 ins,cnt,prevcnt,linenum,bt,field^"\n",size+1,0,eol))

            else if c = #"\"" then  (helper(c,TextIO.input1 ins,cnt,prevcnt,linenum,1-bt,field^String.str(c),size+1,0,eol))

            else (helper(c,TextIO.input1 ins,cnt,prevcnt,linenum,bt,field^String.str(c),size+1,dq,eol))

  in
    if input = nil then raise emptyInputFile
    else helper(#"\n",TextIO.input1 ins,1,1,1,0,"",0,0,"")
  end
    handle UnevenFields(a,b,c) => print("Expected: "^Int.toString(a)^" fields, Present: "^Int.toString(b)^" fields on Line "^Int.toString(c)^"\n");



fun unix2dos(infilename, outfilename) =
        convertNewlines(infilename, "\n", outfilename, "\r\n");

unix2dos("himym.csv","t6.txt");


fun dos2unix(infilename, outfilename) =
        convertNewlines(infilename, "\r\n", outfilename, "\n");

dos2unix("t6.txt","t7.txt");





