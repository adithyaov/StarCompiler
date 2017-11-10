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

  val defList = ref ([]:(string list))
  val defErrors = ref ([]:((string * bool) list))
  val typeErrors = ref ([]:((string * int) list))
  val intTypeList = ref ([]:(string list))
  val stringTypeList = ref ([]:(string list))

  fun isolate [] = []
      | isolate (x::xs) = x::isolate(List.filter (fn y => y <> x) xs) 

  fun showLog () =
    {defList=(isolate (!defList)), defErrors=(isolate (!defErrors)), typeErrors=(isolate (!typeErrors)), intTypeList=(isolate (!intTypeList)), stringTypeList=(isolate (!stringTypeList))}

  val prefix = ref "_jude_"

  structure Ast = StarAst;

  fun isIn [] _ = false
    | isIn (x::xs) y = if (x = y) then true else isIn xs y

  fun reduceType [] sum prod = if sum = 0 andalso prod = 1 then 2 else if sum = 0 andalso prod = 0 then 0 else if sum > 0 andalso prod > 0 then 1 else ~1
    | reduceType (~1::xs) _ _ = ~1
    | reduceType (x::xs) sum prod = reduceType xs (sum + x) (prod * x)

  fun expJS_check listTypes (Ast.IdExp (e, _, _)) = if (isIn (!intTypeList) e) then (1::listTypes) else if (isIn (!stringTypeList) e) then (0::listTypes) else (~1::listTypes)
    | expJS_check listTypes (Ast.IntExp (e, _, _)) = 1::listTypes
    | expJS_check listTypes (Ast.StringExp (e, _, _)) = 0::listTypes
    | expJS_check listTypes (Ast.OpExp (a, b, c, _, _)) = (reduceType (expJS_check [] c) 0 1)::((reduceType (expJS_check [] a) 0 1)::listTypes)
    | expJS_check listTypes _ = listTypes


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

  fun expJS (Ast.IdExp (e, _, _)) =
        let 
          val prefixed = (!prefix) ^ e
          val _ = (defErrors := (prefixed, isIn (!defList) prefixed)::(!defErrors))
        in prefixed end
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

  fun stmtJS (Ast.AssStmt (a, b, _, _)) =
        let 
          val prefixed = (!prefix) ^ a
          val _ = (defList := prefixed::(!defList))
          val rtc = reduceType (expJS_check [] b) 0 1
          val _ = if (rtc = 0) then (stringTypeList := prefixed::(!stringTypeList)) else if (rtc = 1) then (intTypeList := prefixed::(!intTypeList)) else (typeErrors := (!typeErrors))
          val _ = if (rtc = ~1) then (typeErrors := (prefixed, ~1)::(!typeErrors)) else (typeErrors := (prefixed, rtc)::(!typeErrors))
        in prefixed ^ "=" ^ (expJS b) ^ ";" end
    | stmtJS (Ast.MutateStmt (a, b, _, _)) =
        let 
          val prefixed = (!prefix) ^ a
          val _ = (defList := prefixed::(!defList))
          val rtc = reduceType (expJS_check [] b) 0 1
          val _ = if (rtc = ~1) then (typeErrors := (prefixed, ~1)::(!typeErrors)) else (typeErrors := (prefixed, rtc)::(!typeErrors))
        in prefixed ^ "=" ^ (expJS b) ^ ";" end
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
    | blFun [b] = (!prefix) ^ b
    | blFun (b::bs) = 
      let val prefixed = (!prefix) ^ b
      in prefixed ^ ", " ^ (blFun bs) end
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