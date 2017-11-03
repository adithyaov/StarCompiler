structure StarAst =
struct
	type Id_ = string
	type Int_ = int
	type String_ = string

	datatype Start_ = Prog of Prog_

	and Stmt_ = AssStmt of Id_ * Exp_
				| IfStmt of Bool_ * Body_
				| IfElseStmt of Bool_ * Body_ * Body_
				| WhileStmt of Bool_ * Body_
				| PrintStmt of Exp_
				| BREAK 
				| CONTINUE 
				| RETURN

	and Prog_ = ProgPart of Function_ * Prog_
				| ProgPart of Stmt_ * Prog_ 
				| END

	and Body_ = StmtList of Stmt_ * Body_
	        	| StmtLast of Stmt_

	and Bool_ = BoolExp2 of (Exp_ * RelOp_ * Exp_) 
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