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
      let 
      	  fun print_error (s, lNo, lPos) =
	      TextIO.output(TextIO.stdOut, "syntax-error: lineNum: " ^ (Int.toString lNo) ^ ", linePos: "^ (Int.toString lPos) ^", error: "^ s ^ "\n")
       in 
       	  StarParser.parse(0,lexstream,print_error,())
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

  structure Ast = StarAst;


  fun convertArOp (Ast.Plus _) = "+"
    | convertArOp (Ast.Sub _) = "-"
    | convertArOp (Ast.Times _) = "*"
    | convertArOp (Ast.Divide _) = "/"
    | convertArOp (Ast.Carat _) = "^"

  fun convertRelOp (Ast.EqOp _) = "=="
    | convertRelOp (Ast.NeqOp _) = "!="
    | convertRelOp (Ast.LtOp _) = "<"
    | convertRelOp (Ast.LeOp _) = "<="
    | convertRelOp (Ast.GtOp _) = ">"
    | convertRelOp (Ast.GeOp _) = ">="

  fun convertLoOp (Ast.AND _) = "&&"
    | convertLoOp (Ast.OR _) = "||"

  fun expJS (Ast.IdExp (e, _, _)) = e
    | expJS (Ast.StringExp (e, _, _)) = e
    | expJS (Ast.IntExp (e, _, _)) = (Int.toString e)
    | expJS (Ast.CallExp (a, b, _, _)) = (expJS_callExp a b)
    | expJS (Ast.JsExp (a, _, _)) = a
    | expJS (Ast.OpExp (a, b, c, _, _)) = ("(" ^ (expJS a) ^ ")" ^ (convertArOp b) ^ "(" ^ (expJS c) ^ ")")
  and expJS_callExp a b = a ^ "(" ^ (breakList b) ^ ")"
  and breakList [] = ""
    | breakList [b] = expJS b
    | breakList (b::bs) = (expJS b) ^ ", " ^ (breakList bs)

  fun boolJS (Ast.BoolExp2 (a, b, c, _, _)) = (expJS a) ^ (convertRelOp b) ^ (expJS c)
    | boolJS (Ast.BoolExp3 (a, b, c, _, _)) = "(" ^ (boolJS a) ^ ")" ^ (convertLoOp b) ^ "(" ^ (boolJS c) ^ ")"

  fun stmtJS (Ast.AssStmt (a, b, _, _)) = "var " ^ a ^ "=" ^ (expJS b) ^ ";"
    | stmtJS (Ast.MutateStmt (a, b, _, _)) = a ^ "=" ^ (expJS b) ^ ";"
    | stmtJS (Ast.PrintStmt (a, _, _)) = "console.log(" ^ (expJS a) ^ ");"
    | stmtJS (Ast.BREAK _) = "break;"
    | stmtJS (Ast.CONTINUE _) = "continue;"
    | stmtJS (Ast.RETURN _) = "return;"
    | stmtJS (Ast.IfStmt (a, b, _, _)) = "if(" ^ (boolJS a) ^ "){" ^ (stmtJS_body b) ^ "};"
    | stmtJS (Ast.IfElseStmt (a, b, c, _, _)) = "if(" ^ (boolJS a) ^ "){" ^ (stmtJS_body b) ^ "}else{" ^ (stmtJS_body b) ^ "};"
    | stmtJS (Ast.WhileStmt (a, b, _, _)) = "while(" ^ (boolJS a) ^ "){" ^ (stmtJS_body b) ^ "};"
  and stmtJS_body (Ast.StmtList (a, b, _, _)) = (stmtJS a) ^ (stmtJS_body b)
    | stmtJS_body (Ast.StmtLast (a, _, _)) = (stmtJS a)

  fun blFun [] = ""
    | blFun [b] = b
    | blFun (b::bs) = b ^ ", " ^ (blFun bs)
  fun funJS (Ast.Function (a, b, c, _, _)) = "function " ^ a ^ "(" ^ (blFun b) ^ "){" ^ (stmtJS_body c) ^ "}"

  fun progJS (Ast.ProgPart1 (a, b, _, _)) = (funJS a) ^ (progJS b)
    | progJS (Ast.ProgPart2 (a, b, _, _)) = (stmtJS a) ^ (progJS b)
    | progJS (Ast.END _) = ""

  fun startJS (Ast.Prog (a, _, _)) = progJS a

  fun makeJS (outFile, ast) =
    let
      val outStream = TextIO.openOut outFile
      val _ = TextIO.output(outStream, startJS ast)
    in
      TextIO.closeOut outStream
    end;

end