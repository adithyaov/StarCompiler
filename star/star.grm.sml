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
(* Interactive lang transpiler ? *)



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\000\000\
\\001\000\001\000\034\000\000\000\
\\001\000\001\000\035\000\000\000\
\\001\000\001\000\041\000\002\000\021\000\003\000\020\000\000\000\
\\001\000\002\000\021\000\003\000\020\000\000\000\
\\001\000\002\000\059\000\003\000\020\000\000\000\
\\001\000\002\000\076\000\000\000\
\\001\000\004\000\101\000\005\000\101\000\006\000\101\000\007\000\101\000\
\\008\000\101\000\009\000\101\000\010\000\101\000\011\000\101\000\
\\025\000\101\000\027\000\101\000\028\000\101\000\029\000\101\000\
\\031\000\101\000\032\000\101\000\000\000\
\\001\000\006\000\019\000\025\000\017\000\027\000\015\000\028\000\014\000\
\\029\000\013\000\031\000\012\000\032\000\011\000\000\000\
\\001\000\007\000\000\000\008\000\000\000\000\000\
\\001\000\008\000\018\000\026\000\016\000\000\000\
\\001\000\012\000\054\000\013\000\053\000\024\000\052\000\000\000\
\\001\000\012\000\054\000\013\000\053\000\024\000\055\000\000\000\
\\001\000\015\000\044\000\000\000\
\\001\000\016\000\051\000\017\000\050\000\018\000\049\000\019\000\048\000\
\\020\000\047\000\000\000\
\\001\000\021\000\062\000\000\000\
\\001\000\021\000\065\000\000\000\
\\001\000\021\000\074\000\000\000\
\\001\000\021\000\086\000\000\000\
\\001\000\022\000\081\000\000\000\
\\001\000\022\000\082\000\000\000\
\\001\000\022\000\085\000\000\000\
\\001\000\022\000\088\000\000\000\
\\001\000\023\000\032\000\000\000\
\\001\000\023\000\033\000\000\000\
\\001\000\023\000\036\000\000\000\
\\001\000\023\000\043\000\000\000\
\\001\000\024\000\060\000\000\000\
\\001\000\024\000\066\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\000\000\
\\093\000\006\000\019\000\025\000\017\000\027\000\015\000\028\000\014\000\
\\029\000\013\000\031\000\012\000\032\000\011\000\000\000\
\\094\000\006\000\019\000\025\000\017\000\027\000\015\000\028\000\014\000\
\\029\000\013\000\031\000\012\000\032\000\011\000\000\000\
\\095\000\006\000\019\000\025\000\017\000\027\000\015\000\028\000\014\000\
\\029\000\013\000\031\000\012\000\032\000\011\000\000\000\
\\096\000\008\000\018\000\026\000\016\000\000\000\
\\097\000\006\000\019\000\025\000\017\000\027\000\015\000\028\000\014\000\
\\029\000\013\000\031\000\012\000\032\000\011\000\000\000\
\\098\000\002\000\021\000\003\000\020\000\006\000\019\000\008\000\018\000\
\\025\000\017\000\026\000\016\000\027\000\015\000\028\000\014\000\
\\029\000\013\000\031\000\012\000\032\000\011\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\005\000\070\000\009\000\069\000\010\000\068\000\000\000\
\\103\000\009\000\069\000\000\000\
\\104\000\009\000\069\000\000\000\
\\105\000\005\000\070\000\009\000\069\000\010\000\068\000\000\000\
\\106\000\009\000\069\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\004\000\071\000\005\000\070\000\009\000\069\000\010\000\068\000\
\\011\000\067\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\012\000\054\000\013\000\053\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\030\000\084\000\000\000\
\\133\000\000\000\
\"
val actionRowNumbers =
"\037\000\029\000\031\000\034\000\
\\033\000\032\000\030\000\035\000\
\\036\000\052\000\023\000\024\000\
\\051\000\050\000\001\000\002\000\
\\038\000\025\000\054\000\053\000\
\\008\000\008\000\008\000\049\000\
\\008\000\048\000\046\000\039\000\
\\010\000\047\000\003\000\003\000\
\\026\000\013\000\004\000\014\000\
\\057\000\056\000\011\000\055\000\
\\012\000\000\000\005\000\027\000\
\\003\000\064\000\063\000\062\000\
\\061\000\060\000\015\000\008\000\
\\003\000\016\000\028\000\058\000\
\\059\000\007\000\069\000\065\000\
\\008\000\067\000\066\000\008\000\
\\017\000\006\000\006\000\006\000\
\\006\000\006\000\019\000\020\000\
\\008\000\044\000\040\000\043\000\
\\045\000\042\000\041\000\071\000\
\\068\000\021\000\018\000\070\000\
\\008\000\022\000\072\000\009\000"
val gotoT =
"\
\\003\000\087\000\004\000\008\000\005\000\007\000\007\000\006\000\
\\010\000\005\000\011\000\004\000\012\000\003\000\013\000\002\000\
\\014\000\001\000\000\000\
\\000\000\
\\000\000\
\\004\000\024\000\007\000\023\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
\\004\000\024\000\007\000\025\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
\\004\000\024\000\007\000\026\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
\\000\000\
\\005\000\028\000\014\000\027\000\000\000\
\\004\000\024\000\007\000\029\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
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
\\004\000\024\000\007\000\023\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
\\004\000\024\000\007\000\025\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
\\004\000\024\000\007\000\026\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
\\000\000\
\\004\000\024\000\007\000\029\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\028\000\014\000\027\000\000\000\
\\000\000\
\\008\000\038\000\013\000\037\000\015\000\036\000\016\000\035\000\000\000\
\\008\000\040\000\013\000\037\000\015\000\036\000\016\000\035\000\000\000\
\\000\000\
\\000\000\
\\013\000\043\000\000\000\
\\009\000\044\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\054\000\000\000\
\\001\000\056\000\013\000\055\000\000\000\
\\000\000\
\\013\000\037\000\015\000\036\000\016\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\024\000\007\000\061\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
\\008\000\062\000\013\000\037\000\015\000\036\000\016\000\035\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\024\000\007\000\070\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
\\000\000\
\\000\000\
\\004\000\024\000\007\000\071\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
\\000\000\
\\001\000\073\000\000\000\
\\001\000\075\000\000\000\
\\001\000\076\000\000\000\
\\001\000\077\000\000\000\
\\001\000\078\000\000\000\
\\000\000\
\\000\000\
\\004\000\024\000\007\000\081\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
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
\\004\000\024\000\007\000\085\000\010\000\022\000\011\000\021\000\
\\012\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 88
val numrules = 55
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
 | ID of unit ->  (string) | static_ of unit ->  (int)
 | variables_ of unit ->  (int) | program_ of unit ->  (int)
 | constants_ of unit ->  (int) | print_ of unit ->  (int)
 | while_ of unit ->  (int) | assignment_ of unit ->  (int)
 | comparision_ of unit ->  (int) | bool_ of unit ->  (int)
 | body_ of unit ->  (int) | params_ of unit ->  (int)
 | function_ of unit ->  (int) | if_ of unit ->  (int)
 | start of unit ->  (int option) | kwo_ of unit ->  (int)
 | exp_ of unit ->  (int)
end
type svalue = MlyValue.svalue
type result = int option
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 28) => true | (T 26) => true | (T 27) => true | (T 31) => true
 | (T 25) => true | (T 24) => true | (T 30) => true | (T 29) => true
 | (T 5) => true | (T 6) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 3))::
(nil
,nil
 $$ (T 4))::
(nil
,nil
 $$ (T 9))::
(nil
,nil
 $$ (T 10))::
(nil
,nil
 $$ (T 22))::
nil
val noShift = 
fn (T 7) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "STRING"
  | (T 3) => "PLUS"
  | (T 4) => "TIMES"
  | (T 5) => "PRINT"
  | (T 6) => "SEMI"
  | (T 7) => "EOF"
  | (T 8) => "CARAT"
  | (T 9) => "DIV"
  | (T 10) => "SUB"
  | (T 11) => "AND"
  | (T 12) => "OR"
  | (T 13) => "EQ"
  | (T 14) => "ASSIGN"
  | (T 15) => "GE"
  | (T 16) => "GT"
  | (T 17) => "LE"
  | (T 18) => "LT"
  | (T 19) => "NEQ"
  | (T 20) => "LBRACE"
  | (T 21) => "RBRACE"
  | (T 22) => "LPAREN"
  | (T 23) => "RPAREN"
  | (T 24) => "VAR"
  | (T 25) => "FUNCTION"
  | (T 26) => "BREAK"
  | (T 27) => "CONTINUE"
  | (T 28) => "WHILE"
  | (T 29) => "ELSE"
  | (T 30) => "IF"
  | (T 31) => "RETURN"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25)
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18)
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3)end
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
 in (SOME program_)
end)
 in ( LrTable.NT 2, ( result, program_1left, program_1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.body_ body_1, body_1left, body_1right)) :: 
rest671)) => let val  result = MlyValue.start (fn _ => let val  (body_
 as body_1) = body_1 ()
 in (SOME body_)
end)
 in ( LrTable.NT 2, ( result, body_1left, body_1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.constants_ constants_1, constants_1left, 
constants_1right)) :: rest671)) => let val  result = MlyValue.start
 (fn _ => let val  (constants_ as constants_1) = constants_1 ()
 in (SOME constants_)
end)
 in ( LrTable.NT 2, ( result, constants_1left, constants_1right), 
rest671)
end
|  ( 3, ( ( _, ( MlyValue.assignment_ assignment_1, assignment_1left, 
assignment_1right)) :: rest671)) => let val  result = MlyValue.start
 (fn _ => let val  (assignment_ as assignment_1) = assignment_1 ()
 in (SOME assignment_)
end)
 in ( LrTable.NT 2, ( result, assignment_1left, assignment_1right), 
rest671)
end
|  ( 4, ( ( _, ( MlyValue.while_ while_1, while_1left, while_1right))
 :: rest671)) => let val  result = MlyValue.start (fn _ => let val  (
while_ as while_1) = while_1 ()
 in (SOME while_)
end)
 in ( LrTable.NT 2, ( result, while_1left, while_1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.print_ print_1, print_1left, print_1right))
 :: rest671)) => let val  result = MlyValue.start (fn _ => let val  (
print_ as print_1) = print_1 ()
 in (SOME print_)
end)
 in ( LrTable.NT 2, ( result, print_1left, print_1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.function_ function_1, function_1left, 
function_1right)) :: rest671)) => let val  result = MlyValue.start (fn
 _ => let val  (function_ as function_1) = function_1 ()
 in (SOME function_)
end)
 in ( LrTable.NT 2, ( result, function_1left, function_1right), 
rest671)
end
|  ( 7, ( ( _, ( MlyValue.if_ if_1, if_1left, if_1right)) :: rest671))
 => let val  result = MlyValue.start (fn _ => let val  (if_ as if_1) =
 if_1 ()
 in (SOME if_)
end)
 in ( LrTable.NT 2, ( result, if_1left, if_1right), rest671)
end
|  ( 8, ( rest671)) => let val  result = MlyValue.start (fn _ => (NONE
))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( _, EOF1left, EOF1right)) :: rest671)) => let val  
result = MlyValue.program_ (fn _ => (901))
 in ( LrTable.NT 13, ( result, EOF1left, EOF1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.program_ program_1, _, program_1right)) :: 
( _, ( MlyValue.function_ function_1, function_1left, _)) :: rest671))
 => let val  result = MlyValue.program_ (fn _ => let val  function_1 =
 function_1 ()
 val  program_1 = program_1 ()
 in (900)
end)
 in ( LrTable.NT 13, ( result, function_1left, program_1right), 
rest671)
end
|  ( 11, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.exp_ (fn _ => let val  NUM1 = NUM1 ()
 in (0)
end)
 in ( LrTable.NT 0, ( result, NUM1left, NUM1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp_ exp_2, _, exp_2right)) :: _ :: ( _, ( 
MlyValue.exp_ exp_1, exp_1left, _)) :: rest671)) => let val  result = 
MlyValue.exp_ (fn _ => let val  exp_1 = exp_1 ()
 val  exp_2 = exp_2 ()
 in (1)
end)
 in ( LrTable.NT 0, ( result, exp_1left, exp_2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp_ exp_2, _, exp_2right)) :: _ :: ( _, ( 
MlyValue.exp_ exp_1, exp_1left, _)) :: rest671)) => let val  result = 
MlyValue.exp_ (fn _ => let val  exp_1 = exp_1 ()
 val  exp_2 = exp_2 ()
 in (2)
end)
 in ( LrTable.NT 0, ( result, exp_1left, exp_2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp_ exp_2, _, exp_2right)) :: _ :: ( _, ( 
MlyValue.exp_ exp_1, exp_1left, _)) :: rest671)) => let val  result = 
MlyValue.exp_ (fn _ => let val  exp_1 = exp_1 ()
 val  exp_2 = exp_2 ()
 in (3)
end)
 in ( LrTable.NT 0, ( result, exp_1left, exp_2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp_ exp_2, _, exp_2right)) :: _ :: ( _, ( 
MlyValue.exp_ exp_1, exp_1left, _)) :: rest671)) => let val  result = 
MlyValue.exp_ (fn _ => let val  exp_1 = exp_1 ()
 val  exp_2 = exp_2 ()
 in (4)
end)
 in ( LrTable.NT 0, ( result, exp_1left, exp_2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp_ exp_2, _, exp_2right)) :: _ :: ( _, ( 
MlyValue.exp_ exp_1, exp_1left, _)) :: rest671)) => let val  result = 
MlyValue.exp_ (fn _ => let val  exp_1 = exp_1 ()
 val  exp_2 = exp_2 ()
 in (5)
end)
 in ( LrTable.NT 0, ( result, exp_1left, exp_2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.body_ body_1, _, body_1right)) :: ( _, ( 
MlyValue.assignment_ assignment_1, assignment_1left, _)) :: rest671))
 => let val  result = MlyValue.body_ (fn _ => let val  assignment_1 = 
assignment_1 ()
 val  body_1 = body_1 ()
 in (400)
end)
 in ( LrTable.NT 6, ( result, assignment_1left, body_1right), rest671)

end
|  ( 18, ( ( _, ( MlyValue.body_ body_1, _, body_1right)) :: ( _, ( 
MlyValue.if_ if_1, if_1left, _)) :: rest671)) => let val  result = 
MlyValue.body_ (fn _ => let val  if_1 = if_1 ()
 val  body_1 = body_1 ()
 in (401)
end)
 in ( LrTable.NT 6, ( result, if_1left, body_1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.body_ body_1, _, body_1right)) :: ( _, ( 
MlyValue.while_ while_1, while_1left, _)) :: rest671)) => let val  
result = MlyValue.body_ (fn _ => let val  while_1 = while_1 ()
 val  body_1 = body_1 ()
 in (402)
end)
 in ( LrTable.NT 6, ( result, while_1left, body_1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.body_ body_1, _, body_1right)) :: ( _, ( 
MlyValue.print_ print_1, print_1left, _)) :: rest671)) => let val  
result = MlyValue.body_ (fn _ => let val  print_1 = print_1 ()
 val  body_1 = body_1 ()
 in (403)
end)
 in ( LrTable.NT 6, ( result, print_1left, body_1right), rest671)
end
|  ( 21, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let
 val  result = MlyValue.body_ (fn _ => (404))
 in ( LrTable.NT 6, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 22, ( ( _, ( _, CONTINUE1left, CONTINUE1right)) :: rest671)) =>
 let val  result = MlyValue.body_ (fn _ => (405))
 in ( LrTable.NT 6, ( result, CONTINUE1left, CONTINUE1right), rest671)

end
|  ( 23, ( ( _, ( _, RETURN1left, RETURN1right)) :: rest671)) => let
 val  result = MlyValue.body_ (fn _ => (406))
 in ( LrTable.NT 6, ( result, RETURN1left, RETURN1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.NUM NUM1, NUM1left, NUM1right)) :: rest671)
) => let val  result = MlyValue.constants_ (fn _ => let val  NUM1 = 
NUM1 ()
 in (500)
end)
 in ( LrTable.NT 12, ( result, NUM1left, NUM1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.constants_ (fn _ => let
 val  STRING1 = STRING1 ()
 in (501)
end)
 in ( LrTable.NT 12, ( result, STRING1left, STRING1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.variables_ (fn _ => let val  ID1 = ID1 ()
 in (502)
end)
 in ( LrTable.NT 14, ( result, ID1left, ID1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.constants_ constants_1, constants_1left, 
constants_1right)) :: rest671)) => let val  result = MlyValue.static_
 (fn _ => let val  constants_1 = constants_1 ()
 in (999)
end)
 in ( LrTable.NT 15, ( result, constants_1left, constants_1right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.variables_ variables_1, variables_1left, 
variables_1right)) :: rest671)) => let val  result = MlyValue.static_
 (fn _ => let val  variables_1 = variables_1 ()
 in (1000)
end)
 in ( LrTable.NT 15, ( result, variables_1left, variables_1right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.constants_ constants_1, _, constants_1right
)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _))
 :: rest671)) => let val  result = MlyValue.assignment_ (fn _ => let
 val  ID1 = ID1 ()
 val  constants_1 = constants_1 ()
 in (600)
end)
 in ( LrTable.NT 9, ( result, VAR1left, constants_1right), rest671)

end
|  ( 30, ( ( _, ( MlyValue.exp_ exp_1, _, exp_1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let
 val  result = MlyValue.assignment_ (fn _ => let val  ID1 = ID1 ()
 val  exp_1 = exp_1 ()
 in (601)
end)
 in ( LrTable.NT 9, ( result, VAR1left, exp_1right), rest671)
end
|  ( 31, ( ( _, ( _, GE1left, GE1right)) :: rest671)) => let val  
result = MlyValue.comparision_ (fn _ => (2000))
 in ( LrTable.NT 8, ( result, GE1left, GE1right), rest671)
end
|  ( 32, ( ( _, ( _, GT1left, GT1right)) :: rest671)) => let val  
result = MlyValue.comparision_ (fn _ => (2001))
 in ( LrTable.NT 8, ( result, GT1left, GT1right), rest671)
end
|  ( 33, ( ( _, ( _, LE1left, LE1right)) :: rest671)) => let val  
result = MlyValue.comparision_ (fn _ => (2002))
 in ( LrTable.NT 8, ( result, LE1left, LE1right), rest671)
end
|  ( 34, ( ( _, ( _, LT1left, LT1right)) :: rest671)) => let val  
result = MlyValue.comparision_ (fn _ => (2003))
 in ( LrTable.NT 8, ( result, LT1left, LT1right), rest671)
end
|  ( 35, ( ( _, ( _, NEQ1left, NEQ1right)) :: rest671)) => let val  
result = MlyValue.comparision_ (fn _ => (2004))
 in ( LrTable.NT 8, ( result, NEQ1left, NEQ1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.static_ static_2, _, static_2right)) :: ( _
, ( MlyValue.comparision_ comparision_1, _, _)) :: ( _, ( 
MlyValue.static_ static_1, static_1left, _)) :: rest671)) => let val  
result = MlyValue.bool_ (fn _ => let val  static_1 = static_1 ()
 val  comparision_1 = comparision_1 ()
 val  static_2 = static_2 ()
 in (1200)
end)
 in ( LrTable.NT 7, ( result, static_1left, static_2right), rest671)

end
|  ( 37, ( ( _, ( MlyValue.bool_ bool_2, _, bool_2right)) :: _ :: ( _,
 ( MlyValue.bool_ bool_1, bool_1left, _)) :: rest671)) => let val  
result = MlyValue.bool_ (fn _ => let val  bool_1 = bool_1 ()
 val  bool_2 = bool_2 ()
 in (1201)
end)
 in ( LrTable.NT 7, ( result, bool_1left, bool_2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.body_ body_1, _, body_1right)) :: _ :: ( _,
 ( MlyValue.bool_ bool_1, bool_1left, _)) :: rest671)) => let val  
result = MlyValue.bool_ (fn _ => let val  bool_1 = bool_1 ()
 val  body_1 = body_1 ()
 in (1202)
end)
 in ( LrTable.NT 7, ( result, bool_1left, body_1right), rest671)
end
|  ( 39, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.body_ body_1
, _, _)) :: _ :: _ :: ( _, ( MlyValue.bool_ bool_1, _, _)) :: _ :: ( _
, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.while_ (fn _ => let val  bool_1 = bool_1 ()
 val  body_1 = body_1 ()
 in (700)
end)
 in ( LrTable.NT 10, ( result, WHILE1left, RBRACE1right), rest671)
end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.constants_ 
constants_1, _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) =>
 let val  result = MlyValue.print_ (fn _ => let val  constants_1 = 
constants_1 ()
 in (800)
end)
 in ( LrTable.NT 11, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 41, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.body_ body_1
, _, _)) :: _ :: _ :: ( _, ( MlyValue.params_ params_1, _, _)) :: _ ::
 ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: 
rest671)) => let val  result = MlyValue.function_ (fn _ => let val  
ID1 = ID1 ()
 val  params_1 = params_1 ()
 val  body_1 = body_1 ()
 in (300)
end)
 in ( LrTable.NT 4, ( result, FUNCTION1left, RBRACE1right), rest671)

end
|  ( 42, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.body_ body_1
, _, _)) :: _ :: _ :: ( _, ( MlyValue.bool_ bool_1, _, _)) :: _ :: ( _
, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.if_ (fn
 _ => let val  bool_1 = bool_1 ()
 val  body_1 = body_1 ()
 in (200)
end)
 in ( LrTable.NT 3, ( result, IF1left, RBRACE1right), rest671)
end
|  ( 43, ( ( _, ( _, _, RBRACE2right)) :: ( _, ( MlyValue.body_ body_2
, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.body_ body_1, _, _)) :: _
 :: _ :: ( _, ( MlyValue.bool_ bool_1, _, _)) :: _ :: ( _, ( _, 
IF1left, _)) :: rest671)) => let val  result = MlyValue.if_ (fn _ =>
 let val  bool_1 = bool_1 ()
 val  body_1 = body_1 ()
 val  body_2 = body_2 ()
 in (201)
end)
 in ( LrTable.NT 3, ( result, IF1left, RBRACE2right), rest671)
end
|  ( 44, ( ( _, ( _, VAR1left, VAR1right)) :: rest671)) => let val  
result = MlyValue.kwo_ (fn _ => (100))
 in ( LrTable.NT 1, ( result, VAR1left, VAR1right), rest671)
end
|  ( 45, ( ( _, ( _, WHILE1left, WHILE1right)) :: rest671)) => let
 val  result = MlyValue.kwo_ (fn _ => (101))
 in ( LrTable.NT 1, ( result, WHILE1left, WHILE1right), rest671)
end
|  ( 46, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let
 val  result = MlyValue.kwo_ (fn _ => (102))
 in ( LrTable.NT 1, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 47, ( ( _, ( _, CONTINUE1left, CONTINUE1right)) :: rest671)) =>
 let val  result = MlyValue.kwo_ (fn _ => (103))
 in ( LrTable.NT 1, ( result, CONTINUE1left, CONTINUE1right), rest671)

end
|  ( 48, ( ( _, ( _, RETURN1left, RETURN1right)) :: rest671)) => let
 val  result = MlyValue.kwo_ (fn _ => (104))
 in ( LrTable.NT 1, ( result, RETURN1left, RETURN1right), rest671)
end
|  ( 49, ( ( _, ( _, FUNCTION1left, FUNCTION1right)) :: rest671)) =>
 let val  result = MlyValue.kwo_ (fn _ => (105))
 in ( LrTable.NT 1, ( result, FUNCTION1left, FUNCTION1right), rest671)

end
|  ( 50, ( ( _, ( _, VAR1left, VAR1right)) :: rest671)) => let val  
result = MlyValue.kwo_ (fn _ => (106))
 in ( LrTable.NT 1, ( result, VAR1left, VAR1right), rest671)
end
|  ( 51, ( ( _, ( _, IF1left, IF1right)) :: rest671)) => let val  
result = MlyValue.kwo_ (fn _ => (107))
 in ( LrTable.NT 1, ( result, IF1left, IF1right), rest671)
end
|  ( 52, ( ( _, ( _, ELSE1left, ELSE1right)) :: rest671)) => let val  
result = MlyValue.kwo_ (fn _ => (108))
 in ( LrTable.NT 1, ( result, ELSE1left, ELSE1right), rest671)
end
|  ( 53, ( ( _, ( _, PRINT1left, PRINT1right)) :: rest671)) => let
 val  result = MlyValue.kwo_ (fn _ => (109))
 in ( LrTable.NT 1, ( result, PRINT1left, PRINT1right), rest671)
end
|  ( 54, ( ( _, ( _, SEMI1left, SEMI1right)) :: rest671)) => let val  
result = MlyValue.kwo_ (fn _ => (110))
 in ( LrTable.NT 1, ( result, SEMI1left, SEMI1right), rest671)
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
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun CARAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun CONTINUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun RETURN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
end
end
