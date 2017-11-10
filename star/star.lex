structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val linepos = ref 0
val linenum = ref 1
fun eof () = Tokens.EOF(!linenum,!linepos)
fun error (e,lNum, lPos) = TextIO.output (TextIO.stdOut, String.concat["lex-error: lineNum: ", (Int.toString lNum), ",linePos: ", (Int.toString lPos), ",error: ", e, "\n"])

%%
%header (functor StarLexFun(structure Tokens: Star_TOKENS));
alpha=[A-Za-z\.];
all=[A-Za-z0-9\ ];
digit=[0-9];
ws = [\ \t];
%%
\n       => (linenum := (!linenum) + 1; linepos := yypos; lex());
"+"      => (Tokens.PLUS(!linenum, yypos - !linepos));
"*"      => (Tokens.TIMES(!linenum, yypos - !linepos));
";"      => (Tokens.SEMI(!linenum, yypos - !linepos));
","      => (Tokens.COMMA(!linenum, yypos - !linepos));
"-"      => (Tokens.SUB(!linenum, yypos - !linepos));
"^"      => (Tokens.CARAT(!linenum, yypos - !linepos));
"/"      => (Tokens.DIV(!linenum, yypos - !linepos));

"&" => (Tokens.AND(!linenum,yypos - !linepos));
"|" => (Tokens.OR(!linenum, yypos - !linepos));
"==" => (Tokens.EQ(!linenum, yypos - !linepos));
"=" => (Tokens.ASSIGN(!linenum, yypos - !linepos));
">=" => (Tokens.GE(!linenum, yypos - !linepos));
">" => (Tokens.GT(!linenum, yypos - !linepos));
"<=" => (Tokens.LE(!linenum, yypos - !linepos));
"<" => (Tokens.LT(!linenum, yypos - !linepos));
"!=" => (Tokens.NEQ(!linenum, yypos - !linepos));
"{" => (Tokens.LBRACE(!linenum, yypos - !linepos));
"}" => (Tokens.RBRACE(!linenum, yypos - !linepos));
"(" => (Tokens.LPAREN(!linenum, yypos - !linepos));
")" => (Tokens.RPAREN(!linenum, yypos - !linepos));

"js" => (Tokens.JS(!linenum, yypos - !linepos));
"print" => (Tokens.PRINT(!linenum, yypos - !linepos));
"var" => (Tokens.VAR(!linenum, yypos - !linepos));
"function" => (Tokens.FUNCTION(!linenum, yypos - !linepos));
"break" => (Tokens.BREAK(!linenum, yypos - !linepos));
"continue" => (Tokens.CONTINUE(!linenum, yypos - !linepos));
"while" => (Tokens.WHILE(!linenum, yypos - !linepos));
"else" => (Tokens.ELSE(!linenum, yypos - !linepos));
"if" => (Tokens.IF(!linenum, yypos - !linepos));
"return" => (Tokens.RETURN(!linenum, yypos - !linepos));
"end" => (Tokens.END(!linenum, yypos - !linepos));

{ws}+    => (lex());
\"{all}+\" => (Tokens.STRING(yytext,!linenum, yypos - !linepos));
{digit}+ => (Tokens.NUM (valOf (Int.fromString yytext), !linenum, yypos - !linepos));
{alpha}+ => (Tokens.ID(yytext,!linenum, yypos - !linepos));
.      => (error ("ignoring bad character "^yytext, !linenum, yypos - !linepos); 
           Tokens.BOGUS(!linenum, yypos - !linepos));
