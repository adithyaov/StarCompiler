structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
	"line ", (Int.toString l), ": ", e, "\n"
      ])

%%
%header (functor StarLexFun(structure Tokens: Star_TOKENS));
alpha=[A-Za-z];
all=[A-Za-z0-9];
digit=[0-9];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
"+"      => (Tokens.PLUS(!pos,!pos));
"*"      => (Tokens.TIMES(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));
"-"      => (Tokens.SUB(!pos,!pos));
"^"      => (Tokens.CARAT(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));

"&" => (Tokens.AND(!pos,!pos));
"|" => (Tokens.OR(!pos,!pos));
"==" => (Tokens.EQ(!pos,!pos));
"=" => (Tokens.ASSIGN(!pos,!pos));
">=" => (Tokens.GE(!pos,!pos));
">" => (Tokens.GT(!pos,!pos));
"<=" => (Tokens.LE(!pos,!pos));
"<" => (Tokens.LT(!pos,!pos));
"<>" => (Tokens.NEQ(!pos,!pos));
"{" => (Tokens.LBRACE(!pos,!pos));
"}" => (Tokens.RBRACE(!pos,!pos));
"(" => (Tokens.LPAREN(!pos,!pos));
")" => (Tokens.RPAREN(!pos,!pos));
"print" => (Tokens.PRINT(!pos,!pos));

"var" => (Tokens.VAR(!pos,!pos));
"function" => (Tokens.FUNCTION(!pos,!pos));
"break" => (Tokens.BREAK(!pos,!pos));
"continue" => (Tokens.CONTINUE(!pos,!pos));
"while" => (Tokens.WHILE(!pos,!pos));
"else" => (Tokens.ELSE(!pos,!pos));
"if" => (Tokens.IF(!pos,!pos));
"return" => (Tokens.RETURN(!pos,!pos));

{ws}+    => (lex());
:{alpha}+: => (Tokens.STRING(yytext,!pos,!pos));
{digit}+ => (Tokens.NUM (valOf (Int.fromString yytext), !pos, !pos));
{alpha}+ => (Tokens.ID(yytext,!pos,!pos));


"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
lex());