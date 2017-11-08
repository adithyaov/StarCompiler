(* star.sml *)

(* This file provides glue code for building the star session using the
 * parser and lexer specified in star.lex and star.grm.
*)

structure Star = 
struct

(* 
 * We apply the functors generated from star.lex and star.grm to produce
 * the StarParser structure.
 *)

  structure StarLrVals =
    StarLrValsFun(structure Token = LrParser.Token)

  structure StarLex =
    StarLexFun(structure Tokens = StarLrVals.Tokens)

  structure StarParser =
    Join(structure LrParser = LrParser
     structure ParserData = StarLrVals.ParserData
     structure Lex = StarLex)

(* 
 * We need a function which given a lexer invokes the parser. The
 * function invoke does this.
 *)

  fun invoke lexstream =
      let fun print_error (s,i:int,_) =
          TextIO.output(TextIO.stdOut,
                "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")
       in StarParser.parse(0,lexstream,print_error,())
      end

 fun getProper ifNone NONE = ifNone
    | getProper ifNone rest = valOf(rest);

(*  fun parse () = 
      let 
      val lexer = StarParser.makeLexer (fn _ => (getProper "" (TextIO.inputLine TextIO.stdIn)))
      val dummyEOF = StarLrVals.Tokens.EOF(0,0)
      fun loop lexer =
          let val (result,lexer) = invoke lexer
          val (nextToken,lexer) = StarParser.Stream.get lexer
          val _ = case result
                of SOME r => r
           in if StarParser.sameToken(nextToken,dummyEOF) then ()
          else loop lexer
          end
       in loop lexer
      end*)

  fun parse (fileName) = 
      let 
          val inStream = TextIO.openIn fileName;
          val words = String.tokens Char.isSpace o TextIO.inputAll
          val merge = foldr (fn (x, y) => x ^ " " ^ y) ""
          val lexer = StarParser.makeLexer (fn _ => merge (words inStream))
          val (result,lexer) = invoke lexer
          val _ = TextIO.closeIn inStream;
      in 
        result
      end


 (* fun convertToJavascript ast =
    
*)

end