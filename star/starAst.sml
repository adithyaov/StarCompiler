(*structure AST =
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
end;*)

structure StarAst =
struct
	type Id_ = string
	type Int_ = int
	type String_ = string

	datatype Start_ = Prog of Prog_

	and Stmt_ = Stmt of Stmt_
			 | AssStmt of Id_ * Exp_
			 | IfStmt of Bool_ * Body_
			 | IfElseStmt of Bool_ * Body_ * Body_
			 | WhileStmt of Bool_ * Body_
			 | PrintStmt of Exp_
			 | BREAK | CONTINUE | RETURN

	and Prog_ = ProgPart of Function_ * Prog_ | END

	and Body_ = StmtList of Stmt_ * Body_
	        	| StmtLast of Stmt_

	and Bool_ = BoolExp1 of Exp_ 
				| BoolExp2 of (Exp_ * RelOp_ * Exp_) 
				| BoolExp3 of (Bool_ * LoOp_ * Bool_)

	and Exp_ = IdExp of Id_
			| StringExp of String_
	        | IntExp of Int_
	        | CallExp of Id_ * Exp_ list
	        | OpExp of Exp_ * ArOp_ * Exp_

	and ArOp_  = Plus | Minus | Times | Divide | Carat

	and RelOp_ = EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

	and LoOp_  = AND | OR

	and Function_ = Function of Id_ * Id_ list * Body_

end;