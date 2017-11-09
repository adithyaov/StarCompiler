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

  structure Ast = StarAst;

  val defList = ref ([]:(string list))
  val defErrors = ref ([]:((string * bool) list))
  val typeErrors = ref ([]:((string * int) list))
  val intTypeList = ref ([]:(string list))
  val stringTypeList = ref ([]:(string list))

  fun convertArOp Ast.Plus = "+"
    | convertArOp Ast.Minus = "-"
    | convertArOp Ast.Times = "*"
    | convertArOp Ast.Divide = "/"
    | convertArOp Ast.Carat = "^"

  fun convertRelOp Ast.EqOp = "=="
    | convertRelOp Ast.NeqOp = "!="
    | convertRelOp Ast.LtOp = "<"
    | convertRelOp Ast.LeOp = "<="
    | convertRelOp Ast.GtOp = ">"
    | convertRelOp Ast.GeOp = ">="

  fun convertLoOp Ast.AND = "&&"
    | convertLoOp Ast.OR = "||"

  fun isIn [] _ = false
    | isIn (x::xs) y = if (x = y) then true else isIn xs y


  fun reduceType [] sum prod = if sum = 0 andalso prod = 0 then 0 else if sum > 0 andalso prod > 0 then 1 else ~1
    | reduceType (~1::xs) _ _ = ~1
    | reduceType (x::xs) sum prod = reduceType xs (sum + x) (prod * x)

  fun expJS_check listTypes (Ast.IdExp e) = if (isIn (!intTypeList) e) then (1::listTypes) else if (isIn (!stringTypeList) e) then (0::listTypes) else (~1::listTypes)
    | expJS_check listTypes (Ast.IntExp e) = 1::listTypes
    | expJS_check listTypes (Ast.StringExp e) = 0::listTypes
    | expJS_check listTypes (Ast.OpExp (a, b, c)) = (reduceType (expJS_check [] c) 0 1)::((reduceType (expJS_check [] a) 0 1)::listTypes)
    | expJS_check listTypes _ = listTypes

  fun expJS (Ast.IdExp e) = 
        let val _ = (defErrors := (e, isIn (!defList) e)::(!defErrors))
        in e end
    | expJS (Ast.StringExp e) = e
    | expJS (Ast.IntExp e) = (Int.toString e)
    | expJS (Ast.CallExp (a, b)) = (expJS_callExp a b)
    | expJS (Ast.OpExp (a, b, c)) = ("(" ^ (expJS a) ^ ")" ^ (convertArOp b) ^ "(" ^ (expJS c) ^ ")")
  and expJS_callExp a b = a ^ "(" ^ (breakList b) ^ ")"
  and breakList [] = ""
    | breakList [b] = expJS b
    | breakList (b::bs) = (expJS b) ^ ", " ^ (breakList bs)

  fun boolJS (Ast.BoolExp2 (a, b, c)) = (expJS a) ^ (convertRelOp b) ^ (expJS c)
    | boolJS (Ast.BoolExp3 (a, b, c)) = "(" ^ (boolJS a) ^ ")" ^ (convertLoOp b) ^ "(" ^ (boolJS c) ^ ")"

  fun stmtJS (Ast.AssStmt (a, b)) = 
        let 
          val _ = (defList := a::(!defList))
          val rtc = reduceType (expJS_check [] b) 0 1
          val _ = if (rtc = ~1) then (typeErrors := (a, ~1)::(!typeErrors)) else (typeErrors := (a, rtc)::(!typeErrors))
        in a ^ "=" ^ (expJS b) ^ ";" end
    | stmtJS (Ast.MutateStmt (a, b)) = 
        let 
          val _ = (defList := a::(!defList))
          val rtc = reduceType (expJS_check [] b) 0 1
          val _ = if (rtc = ~1) then (typeErrors := (a, ~1)::(!typeErrors)) else (typeErrors := (a, rtc)::(!typeErrors))
        in a ^ "=" ^ (expJS b) ^ ";" end
    | stmtJS (Ast.PrintStmt (a)) = "console.log(" ^ (expJS a) ^ ");"
    | stmtJS Ast.BREAK = "break;"
    | stmtJS Ast.CONTINUE = "continue;"
    | stmtJS Ast.RETURN = "return;"
    | stmtJS (Ast.IfStmt (a, b)) = "if(" ^ (boolJS a) ^ "){" ^ (stmtJS_body b) ^ "};"
    | stmtJS (Ast.IfElseStmt (a, b, c)) = "if(" ^ (boolJS a) ^ "){" ^ (stmtJS_body b) ^ "}else{" ^ (stmtJS_body b) ^ "};"
    | stmtJS (Ast.WhileStmt (a, b)) = "while(" ^ (boolJS a) ^ "){" ^ (stmtJS_body b) ^ "};"
  and stmtJS_body (Ast.StmtList (a, b)) = (stmtJS a) ^ (stmtJS_body b)
    | stmtJS_body (Ast.StmtLast (a)) = (stmtJS a)

  fun blFun [] = ""
    | blFun [b] = b
    | blFun (b::bs) = b ^ ", " ^ (blFun bs)
  fun funJS (Ast.Function (a, b, c)) = "function " ^ a ^ "(" ^ (blFun b) ^ "){" ^ (stmtJS_body c) ^ "}"

  fun progJS (Ast.ProgPart1 (a, b)) = (funJS a) ^ (progJS b)
    | progJS (Ast.ProgPart2 (a, b)) = (stmtJS a) ^ (progJS b)
    | progJS (Ast.END) = ""

  fun startJS (Ast.Prog (a)) = progJS a

  fun makeJS (outFile, ast) =
    let
      val outStream = TextIO.openOut outFile
      val _ = TextIO.output(outStream, startJS ast)
    in
      TextIO.closeOut outStream
    end;

end