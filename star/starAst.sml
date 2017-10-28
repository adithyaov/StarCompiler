structure AST =
struct

type pos = int

type var = string * pos

datatype start = Prog of prog
			   | Exp of exp
			   | Stmt of stmt

and stmt = AssStmt of {var: var, exp: exp, pos: pos}
		 | IfStmt of {test: bool_, if_: body, else_: body option, pos: pos}
		 | WhileStmt of {test: bool_, body: body, pos: pos}
		 | PrintStmt of exp

and prog = FunctionDec of fundec * prog | END

and body = StmList of stmt * body
        | Break of pos * body
        | Continue of pos * body
        | Return of pos * body
        | Empty 

and bool_ = BoolExp1 of exp | BoolExp2 of (exp * relOp * exp) | BoolExp3 of (bool_ * loOp * bool_)

and exp = VarExp of var
        | IntExp of int
        | CallExp of {func: string, params: exp list, pos: pos}
        | OpExp of {left: exp, oper: arOp, right: exp, pos: pos}
        | SeqExp of (exp * pos) list

and arOp  = Plus | Minus | Times | Divide | Carat

and relOp = EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

and loOp  = AND | OR

withtype   fundec = {name: string,
		   params: var list,
		   body: exp,
		   pos: pos}
end;