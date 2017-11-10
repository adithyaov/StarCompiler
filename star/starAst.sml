structure StarAst =
struct
    type LPos_ = int
    type LNum_ = int
    type Id_ = string
    type Int_ = int
    type String_ = string

    datatype Start_ = Prog of Prog_ * LNum_ * LPos_

    and Stmt_ = AssStmt    of Id_ * Exp_ * LNum_ * LPos_
    		  | MutateStmt of Id_ * Exp_ * LNum_ * LPos_
              | IfStmt     of Bool_ * Body_ * LNum_ * LPos_
              | IfElseStmt of Bool_ * Body_ * Body_ * LNum_ * LPos_
              | WhileStmt  of Bool_ * Body_ * LNum_ * LPos_
              | PrintStmt  of Exp_ * LNum_ * LPos_
              | BREAK      of LNum_ * LPos_
              | CONTINUE   of LNum_ * LPos_
              | RETURN     of LNum_ * LPos_

    and Prog_ = ProgPart1 of Function_ * Prog_ * LNum_ * LPos_
              | ProgPart2 of Stmt_ * Prog_ * LNum_ * LPos_
              | END       of LNum_ * LPos_

    and Body_ = StmtList of Stmt_ * Body_ * LNum_ * LPos_
              | StmtLast of Stmt_ * LNum_ * LPos_

    and Bool_ = BoolExp2 of Exp_ * RelOp_ * Exp_ * LNum_ * LPos_
              | BoolExp3 of Bool_ * LoOp_ * Bool_ * LNum_ * LPos_

    and Exp_ = IdExp     of Id_ * LNum_ * LPos_
             | StringExp of String_ * LNum_ * LPos_
             | IntExp    of Int_ * LNum_ * LPos_
             | CallExp   of Id_ * Exp_ list * LNum_ * LPos_
             | OpExp     of Exp_ * ArOp_ * Exp_ * LNum_ * LPos_
             | JsExp     of Id_ * LNum_ * LPos_

    and ArOp_  = Plus   of LNum_ * LPos_
               | Sub    of LNum_ * LPos_
               | Times  of LNum_ * LPos_
               | Divide of LNum_ * LPos_
               | Carat  of LNum_ * LPos_

    and RelOp_ = EqOp   of LNum_ * LPos_
               | NeqOp  of LNum_ * LPos_
               | LtOp   of LNum_ * LPos_
               | LeOp   of LNum_ * LPos_
               | GtOp   of LNum_ * LPos_
               | GeOp   of LNum_ * LPos_

    and LoOp_  = AND    of LNum_ * LPos_
               | OR     of LNum_ * LPos_

    and Function_ = Function of Id_ * Id_ list * Body_ * LNum_ * LPos_

end;