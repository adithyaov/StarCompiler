functor StarLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Star_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Interactive lang transpiler ? :-) *)

structure Ast = StarAst


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\008\000\000\000\
\\001\000\001\000\011\000\000\000\
\\001\000\001\000\033\000\000\000\
\\001\000\001\000\039\000\002\000\038\000\003\000\037\000\000\000\
\\001\000\004\000\058\000\005\000\057\000\010\000\056\000\011\000\055\000\
\\012\000\054\000\025\000\061\000\000\000\
\\001\000\006\000\028\000\026\000\027\000\028\000\026\000\029\000\025\000\
\\030\000\024\000\032\000\023\000\033\000\022\000\000\000\
\\001\000\008\000\000\000\009\000\000\000\000\000\
\\001\000\013\000\046\000\014\000\045\000\025\000\044\000\000\000\
\\001\000\013\000\046\000\014\000\045\000\025\000\059\000\000\000\
\\001\000\016\000\041\000\000\000\
\\001\000\022\000\014\000\000\000\
\\001\000\022\000\063\000\000\000\
\\001\000\022\000\070\000\000\000\
\\001\000\022\000\076\000\000\000\
\\001\000\023\000\030\000\000\000\
\\001\000\023\000\073\000\000\000\
\\001\000\023\000\074\000\000\000\
\\001\000\023\000\078\000\000\000\
\\001\000\024\000\009\000\000\000\
\\001\000\024\000\031\000\000\000\
\\001\000\024\000\032\000\000\000\
\\001\000\024\000\034\000\000\000\
\\001\000\025\000\012\000\000\000\
\\001\000\027\000\006\000\034\000\005\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\005\000\057\000\010\000\056\000\011\000\055\000\000\000\
\\094\000\010\000\056\000\000\000\
\\095\000\010\000\056\000\000\000\
\\096\000\005\000\057\000\010\000\056\000\011\000\055\000\000\000\
\\097\000\010\000\056\000\000\000\
\\098\000\000\000\
\\099\000\006\000\028\000\026\000\027\000\028\000\026\000\029\000\025\000\
\\030\000\024\000\032\000\023\000\033\000\022\000\000\000\
\\100\000\004\000\058\000\005\000\057\000\010\000\056\000\011\000\055\000\
\\012\000\054\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\004\000\058\000\005\000\057\000\010\000\056\000\011\000\055\000\
\\012\000\054\000\015\000\053\000\017\000\052\000\018\000\051\000\
\\019\000\050\000\020\000\049\000\021\000\048\000\000\000\
\\110\000\004\000\058\000\005\000\057\000\010\000\056\000\011\000\055\000\
\\012\000\054\000\000\000\
\\111\000\013\000\046\000\014\000\045\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\007\000\013\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\120\000\031\000\075\000\000\000\
\\121\000\000\000\
\"
val actionRowNumbers =
"\023\000\024\000\023\000\032\000\
\\000\000\033\000\018\000\001\000\
\\022\000\058\000\010\000\001\000\
\\005\000\059\000\043\000\028\000\
\\027\000\025\000\014\000\026\000\
\\031\000\019\000\020\000\030\000\
\\029\000\002\000\021\000\042\000\
\\060\000\003\000\003\000\009\000\
\\003\000\007\000\053\000\035\000\
\\034\000\036\000\008\000\003\000\
\\004\000\003\000\011\000\052\000\
\\051\000\003\000\049\000\048\000\
\\047\000\046\000\045\000\050\000\
\\003\000\003\000\003\000\003\000\
\\003\000\012\000\044\000\057\000\
\\055\000\005\000\054\000\040\000\
\\039\000\041\000\038\000\037\000\
\\005\000\015\000\016\000\061\000\
\\056\000\013\000\005\000\017\000\
\\062\000\006\000"
val gotoT =
"\
\\002\000\077\000\004\000\002\000\012\000\001\000\000\000\
\\000\000\
\\004\000\002\000\012\000\005\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\008\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\013\000\000\000\
\\003\000\019\000\006\000\018\000\009\000\017\000\010\000\016\000\
\\011\000\015\000\013\000\014\000\000\000\
\\000\000\
\\003\000\019\000\006\000\027\000\009\000\017\000\010\000\016\000\
\\011\000\015\000\013\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\034\000\007\000\033\000\000\000\
\\001\000\034\000\007\000\038\000\000\000\
\\000\000\
\\001\000\040\000\000\000\
\\016\000\041\000\000\000\
\\008\000\045\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\041\000\000\000\
\\001\000\058\000\000\000\
\\000\000\
\\001\000\034\000\007\000\060\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\062\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\063\000\000\000\
\\001\000\064\000\000\000\
\\001\000\065\000\000\000\
\\001\000\066\000\000\000\
\\001\000\067\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\041\000\000\000\
\\003\000\019\000\006\000\069\000\009\000\017\000\010\000\016\000\
\\011\000\015\000\013\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\019\000\006\000\070\000\009\000\017\000\010\000\016\000\
\\011\000\015\000\013\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\019\000\006\000\075\000\009\000\017\000\010\000\016\000\
\\011\000\015\000\013\000\014\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 78
val numrules = 42
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | NUM of unit ->  (int)
 | ID of unit ->  (string) | logic_ of unit ->  (Ast.LoOp_)
 | functionCall_ of unit ->  (Ast.Exp_)
 | functionCallParams_ of unit ->  (Ast.Exp_ list)
 | stm_ of unit ->  (Ast.Stmt_) | program_ of unit ->  (Ast.Prog_)
 | print_ of unit ->  (Ast.Stmt_) | while_ of unit ->  (Ast.Stmt_)
 | assignment_ of unit ->  (Ast.Stmt_)
 | comparison_ of unit ->  (Ast.RelOp_)
 | bool_ of unit ->  (Ast.Bool_) | body_ of unit ->  (Ast.Body_)
 | params_ of unit ->  (string list)
 | function_ of unit ->  (Ast.Function_) | if_ of unit ->  (Ast.Stmt_)
 | start of unit ->  (Ast.Start_ option) | exp_ of unit ->  (Ast.Exp_)
end
type svalue = MlyValue.svalue
type result = Ast.Start_ option
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 29) => true | (T 27) => true | (T 28) => true | (T 32) => true
 | (T 26) => true | (T 25) => true | (T 31) => true | (T 30) => true
 | (T 5) => true | (T 7) => true | (T 33) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 3))::
(nil
,nil
 $$ (T 4))::
(nil
,nil
 $$ (T 10))::
(nil
,nil
 $$ (T 11))::
(nil
,nil
 $$ (T 23))::
nil
val noShift = 
fn (T 8) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "STRING"
  | (T 3) => "PLUS"
  | (T 4) => "TIMES"
  | (T 5) => "PRINT"
  | (T 6) => "COMMA"
  | (T 7) => "SEMI"
  | (T 8) => "EOF"
  | (T 9) => "CARAT"
  | (T 10) => "DIV"
  | (T 11) => "SUB"
  | (T 12) => "AND"
  | (T 13) => "OR"
  | (T 14) => "EQ"
  | (T 15) => "ASSIGN"
  | (T 16) => "GE"
  | (T 17) => "GT"
  | (T 18) => "LE"
  | (T 19) => "LT"
  | (T 20) => "NEQ"
  | (T 21) => "LBRACE"
  | (T 22) => "RBRACE"
  | (T 23) => "LPAREN"
  | (T 24) => "RPAREN"
  | (T 25) => "VAR"
  | (T 26) => "FUNCTION"
  | (T 27) => "BREAK"
  | (T 28) => "CONTINUE"
  | (T 29) => "WHILE"
  | (T 30) => "ELSE"
  | (T 31) => "IF"
  | (T 32) => "RETURN"
  | (T 33) => "END"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ 
(T 5) $$ (T 4) $$ (T 3)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.program_ program_1, program_1left, 
program_1right)) :: rest671)) => let val  result = MlyValue.start (fn
 _ => let val  (program_ as program_1) = program_1 ()
 in (SOME (Ast.Prog(program_)))
end)
 in ( LrTable.NT 1, ( result, program_1left, program_1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.assignment_ assignment_1, assignment_1left, 
assignment_1right)) :: rest671)) => let val  result = MlyValue.stm_
 (fn _ => let val  (assignment_ as assignment_1) = assignment_1 ()
 in (Ast.Stmt(assignment_))
end)
 in ( LrTable.NT 12, ( result, assignment_1left, assignment_1right), 
rest671)
end
|  ( 2, ( ( _, ( MlyValue.if_ if_1, if_1left, if_1right)) :: rest671))
 => let val  result = MlyValue.stm_ (fn _ => let val  (if_ as if_1) = 
if_1 ()
 in (Ast.Stmt(if_))
end)
 in ( LrTable.NT 12, ( result, if_1left, if_1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.while_ while_1, while_1left, while_1right))
 :: rest671)) => let val  result = MlyValue.stm_ (fn _ => let val  (
while_ as while_1) = while_1 ()
 in (Ast.Stmt(while_))
end)
 in ( LrTable.NT 12, ( result, while_1left, while_1right), rest671)

end
|  ( 4, ( ( _, ( MlyValue.print_ print_1, print_1left, print_1right))
 :: rest671)) => let val  result = MlyValue.stm_ (fn _ => let val  (
print_ as print_1) = print_1 ()
 in (Ast.Stmt(print_))
end)
 in ( LrTable.NT 12, ( result, print_1left, print_1right), rest671)

end
|  ( 5, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let val 
 result = MlyValue.stm_ (fn _ => (Ast.Stmt(Ast.BREAK)))
 in ( LrTable.NT 12, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 6, ( ( _, ( _, CONTINUE1left, CONTINUE1right)) :: rest671)) =>
 let val  result = MlyValue.stm_ (fn _ => (Ast.Stmt(Ast.CONTINUE)))
 in ( LrTable.NT 12, ( result, CONTINUE1left, CONTINUE1right), rest671
)
end
|  ( 7, ( ( _, ( _, RETURN1left, RETURN1right)) :: rest671)) => let
 val  result = MlyValue.stm_ (fn _ => (Ast.Stmt(Ast.RETURN)))
 in ( LrTable.NT 12, ( result, RETURN1left, RETURN1right), rest671)

end
|  ( 8, ( ( _, ( _, END1left, END1right)) :: rest671)) => let val  
result = MlyValue.program_ (fn _ => (Ast.END))
 in ( LrTable.NT 11, ( result, END1left, END1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.program_ program_1, _, program_1right)) :: (
 _, ( MlyValue.function_ function_1, function_1left, _)) :: rest671))
 => let val  result = MlyValue.program_ (fn _ => let val  (function_
 as function_1) = function_1 ()
 val  (program_ as program_1) = program_1 ()
 in (Ast.ProgPart(function_, program_))
end)
 in ( LrTable.NT 11, ( result, function_1left, program_1right), 
rest671)
end
|  ( 10, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.exp_ (fn _ => let val  (NUM as NUM1) =
 NUM1 ()
 in (Ast.IntExp(NUM))
end)
 in ( LrTable.NT 0, ( result, NUM1left, NUM1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.exp_ (fn _ => let val  (
STRING as STRING1) = STRING1 ()
 in (Ast.StringExp(STRING))
end)
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.exp_ (fn _ => let val  (ID as ID1) = ID1
 ()
 in (Ast.IdExp(ID))
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp_ exp_2, _, exp_2right)) :: _ :: ( _, ( 
MlyValue.exp_ exp_1, exp_1left, _)) :: rest671)) => let val  result = 
MlyValue.exp_ (fn _ => let val  exp_1 = exp_1 ()
 val  exp_2 = exp_2 ()
 in (Ast.OpExp(exp_1, Ast.Plus, exp_2))
end)
 in ( LrTable.NT 0, ( result, exp_1left, exp_2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp_ exp_2, _, exp_2right)) :: _ :: ( _, ( 
MlyValue.exp_ exp_1, exp_1left, _)) :: rest671)) => let val  result = 
MlyValue.exp_ (fn _ => let val  exp_1 = exp_1 ()
 val  exp_2 = exp_2 ()
 in (Ast.OpExp(exp_1, Ast.Times, exp_2))
end)
 in ( LrTable.NT 0, ( result, exp_1left, exp_2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp_ exp_2, _, exp_2right)) :: _ :: ( _, ( 
MlyValue.exp_ exp_1, exp_1left, _)) :: rest671)) => let val  result = 
MlyValue.exp_ (fn _ => let val  exp_1 = exp_1 ()
 val  exp_2 = exp_2 ()
 in (Ast.OpExp(exp_1, Ast.Divide, exp_2))
end)
 in ( LrTable.NT 0, ( result, exp_1left, exp_2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp_ exp_2, _, exp_2right)) :: _ :: ( _, ( 
MlyValue.exp_ exp_1, exp_1left, _)) :: rest671)) => let val  result = 
MlyValue.exp_ (fn _ => let val  exp_1 = exp_1 ()
 val  exp_2 = exp_2 ()
 in (Ast.OpExp(exp_1, Ast.Minus, exp_2))
end)
 in ( LrTable.NT 0, ( result, exp_1left, exp_2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp_ exp_2, _, exp_2right)) :: _ :: ( _, ( 
MlyValue.exp_ exp_1, exp_1left, _)) :: rest671)) => let val  result = 
MlyValue.exp_ (fn _ => let val  exp_1 = exp_1 ()
 val  exp_2 = exp_2 ()
 in (Ast.OpExp(exp_1, Ast.Carat, exp_2))
end)
 in ( LrTable.NT 0, ( result, exp_1left, exp_2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.body_ body_1, _, body_1right)) :: ( _, ( 
MlyValue.stm_ stm_1, stm_1left, _)) :: rest671)) => let val  result = 
MlyValue.body_ (fn _ => let val  (stm_ as stm_1) = stm_1 ()
 val  (body_ as body_1) = body_1 ()
 in (Ast.StmtList(stm_, body_))
end)
 in ( LrTable.NT 5, ( result, stm_1left, body_1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.stm_ stm_1, stm_1left, stm_1right)) :: 
rest671)) => let val  result = MlyValue.body_ (fn _ => let val  (stm_
 as stm_1) = stm_1 ()
 in (Ast.StmtLast(stm_))
end)
 in ( LrTable.NT 5, ( result, stm_1left, stm_1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp_ exp_1, _, exp_1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let
 val  result = MlyValue.assignment_ (fn _ => let val  (ID as ID1) = 
ID1 ()
 val  (exp_ as exp_1) = exp_1 ()
 in (Ast.AssStmt(ID, exp_))
end)
 in ( LrTable.NT 8, ( result, VAR1left, exp_1right), rest671)
end
|  ( 21, ( ( _, ( _, GE1left, GE1right)) :: rest671)) => let val  
result = MlyValue.comparison_ (fn _ => (Ast.GeOp))
 in ( LrTable.NT 7, ( result, GE1left, GE1right), rest671)
end
|  ( 22, ( ( _, ( _, GT1left, GT1right)) :: rest671)) => let val  
result = MlyValue.comparison_ (fn _ => (Ast.GtOp))
 in ( LrTable.NT 7, ( result, GT1left, GT1right), rest671)
end
|  ( 23, ( ( _, ( _, LE1left, LE1right)) :: rest671)) => let val  
result = MlyValue.comparison_ (fn _ => (Ast.LeOp))
 in ( LrTable.NT 7, ( result, LE1left, LE1right), rest671)
end
|  ( 24, ( ( _, ( _, LT1left, LT1right)) :: rest671)) => let val  
result = MlyValue.comparison_ (fn _ => (Ast.LtOp))
 in ( LrTable.NT 7, ( result, LT1left, LT1right), rest671)
end
|  ( 25, ( ( _, ( _, NEQ1left, NEQ1right)) :: rest671)) => let val  
result = MlyValue.comparison_ (fn _ => (Ast.NeqOp))
 in ( LrTable.NT 7, ( result, NEQ1left, NEQ1right), rest671)
end
|  ( 26, ( ( _, ( _, EQ1left, EQ1right)) :: rest671)) => let val  
result = MlyValue.comparison_ (fn _ => (Ast.EqOp))
 in ( LrTable.NT 7, ( result, EQ1left, EQ1right), rest671)
end
|  ( 27, ( ( _, ( _, AND1left, AND1right)) :: rest671)) => let val  
result = MlyValue.logic_ (fn _ => (Ast.AND))
 in ( LrTable.NT 15, ( result, AND1left, AND1right), rest671)
end
|  ( 28, ( ( _, ( _, OR1left, OR1right)) :: rest671)) => let val  
result = MlyValue.logic_ (fn _ => (Ast.OR))
 in ( LrTable.NT 15, ( result, OR1left, OR1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.exp_ exp_1, exp_1left, exp_1right)) :: 
rest671)) => let val  result = MlyValue.bool_ (fn _ => let val  (exp_
 as exp_1) = exp_1 ()
 in (Ast.BoolExp1(exp_))
end)
 in ( LrTable.NT 6, ( result, exp_1left, exp_1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.exp_ exp_2, _, exp_2right)) :: ( _, ( 
MlyValue.comparison_ comparison_1, _, _)) :: ( _, ( MlyValue.exp_ 
exp_1, exp_1left, _)) :: rest671)) => let val  result = MlyValue.bool_
 (fn _ => let val  exp_1 = exp_1 ()
 val  (comparison_ as comparison_1) = comparison_1 ()
 val  exp_2 = exp_2 ()
 in (Ast.BoolExp2(exp_1, comparison_, exp_2))
end)
 in ( LrTable.NT 6, ( result, exp_1left, exp_2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.bool_ bool_2, _, bool_2right)) :: ( _, ( 
MlyValue.logic_ logic_1, _, _)) :: ( _, ( MlyValue.bool_ bool_1, 
bool_1left, _)) :: rest671)) => let val  result = MlyValue.bool_ (fn _
 => let val  bool_1 = bool_1 ()
 val  (logic_ as logic_1) = logic_1 ()
 val  bool_2 = bool_2 ()
 in (Ast.BoolExp3(bool_1, logic_, bool_2))
end)
 in ( LrTable.NT 6, ( result, bool_1left, bool_2right), rest671)
end
|  ( 32, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.body_ body_1
, _, _)) :: _ :: _ :: ( _, ( MlyValue.bool_ bool_1, _, _)) :: _ :: ( _
, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.while_ (fn _ => let val  (bool_ as bool_1) = bool_1 ()
 val  (body_ as body_1) = body_1 ()
 in (Ast.WhileStmt(bool_, body_))
end)
 in ( LrTable.NT 9, ( result, WHILE1left, RBRACE1right), rest671)
end
|  ( 33, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp_ exp_1,
 _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) => let val  
result = MlyValue.print_ (fn _ => let val  (exp_ as exp_1) = exp_1 ()
 in (Ast.PrintStmt(exp_))
end)
 in ( LrTable.NT 10, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.params_ (fn _ => let val  (ID as ID1) = 
ID1 ()
 in ([ID])
end)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.params_ params_1, _, params_1right)) :: _
 :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  
result = MlyValue.params_ (fn _ => let val  (ID as ID1) = ID1 ()
 val  (params_ as params_1) = params_1 ()
 in (ID::params_)
end)
 in ( LrTable.NT 4, ( result, ID1left, params_1right), rest671)
end
|  ( 36, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.body_ body_1
, _, _)) :: _ :: _ :: ( _, ( MlyValue.params_ params_1, _, _)) :: _ ::
 ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: 
rest671)) => let val  result = MlyValue.function_ (fn _ => let val  (
ID as ID1) = ID1 ()
 val  (params_ as params_1) = params_1 ()
 val  (body_ as body_1) = body_1 ()
 in (Ast.Function(ID, params_, body_))
end)
 in ( LrTable.NT 3, ( result, FUNCTION1left, RBRACE1right), rest671)

end
|  ( 37, ( ( _, ( MlyValue.exp_ exp_1, exp_1left, exp_1right)) :: 
rest671)) => let val  result = MlyValue.functionCallParams_ (fn _ =>
 let val  (exp_ as exp_1) = exp_1 ()
 in ([exp_])
end)
 in ( LrTable.NT 13, ( result, exp_1left, exp_1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.functionCallParams_ functionCallParams_1, _
, functionCallParams_1right)) :: _ :: ( _, ( MlyValue.exp_ exp_1, 
exp_1left, _)) :: rest671)) => let val  result = 
MlyValue.functionCallParams_ (fn _ => let val  (exp_ as exp_1) = exp_1
 ()
 val  (functionCallParams_ as functionCallParams_1) = 
functionCallParams_1 ()
 in (exp_::functionCallParams_)
end)
 in ( LrTable.NT 13, ( result, exp_1left, functionCallParams_1right), 
rest671)
end
|  ( 39, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( 
MlyValue.functionCallParams_ functionCallParams_1, _, _)) :: _ :: ( _,
 ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.functionCall_ (fn _ => let val  (ID as ID1) = ID1 ()
 val  (functionCallParams_ as functionCallParams_1) = 
functionCallParams_1 ()
 in (Ast.CallExp(ID, functionCallParams_))
end)
 in ( LrTable.NT 14, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 40, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.body_ body_1
, _, _)) :: _ :: _ :: ( _, ( MlyValue.bool_ bool_1, _, _)) :: _ :: ( _
, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.if_ (fn
 _ => let val  (bool_ as bool_1) = bool_1 ()
 val  (body_ as body_1) = body_1 ()
 in (Ast.IfStmt(bool_, body_))
end)
 in ( LrTable.NT 2, ( result, IF1left, RBRACE1right), rest671)
end
|  ( 41, ( ( _, ( _, _, RBRACE2right)) :: ( _, ( MlyValue.body_ body_2
, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.body_ body_1, _, _)) :: _
 :: _ :: ( _, ( MlyValue.bool_ bool_1, _, _)) :: _ :: ( _, ( _, 
IF1left, _)) :: rest671)) => let val  result = MlyValue.if_ (fn _ =>
 let val  (bool_ as bool_1) = bool_1 ()
 val  (body_ as body_1) = body_1 ()
 val  body_2 = body_2 ()
 in (Ast.IfElseStmt(bool_, body_, body_))
end)
 in ( LrTable.NT 2, ( result, IF1left, RBRACE2right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Star_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun CARAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun CONTINUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun RETURN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
end
end
