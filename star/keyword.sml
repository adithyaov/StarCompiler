structure KeyWord :sig val find:string->(int*int->(svalue,int) token) option end = struct
 	val TableSize = 422 (* 211 *)
    
    val HashFactor = 5
 	
 	val hash = fn s => List.foldr (fn (c,v) => (v * HashFactor + (ord c)) mod TableSize) 0 (explode s)
 	
 	val HashTable = Array.array(TableSize,nil) :(string * (int * int -> (svalue,int) token)) list Array.array
 	
 	val add = fn (s,v) => let
 							 val i = hash s
 					      in
 					      	 Array.update(HashTable,i,(s,v)::(Array.sub(HashTable, i)))
 						  end

 	val find = fn s => let 
 						  val i = hash s 
 						  fun f ((key,v)::r) = if s=key then SOME v	else f r 
 						    | f          nil = NONE
 					   in
 					   	  f (Array.sub(HashTable, i))
 					   end

 	val _ = (List.app add [
    ("var", Tokens.VAR)
 	("function", Tokens.FUNCTION),
 	("break", Tokens.BREAK),
 	("continue", Tokens.CONTINUE),
 	("while", (Tokens.WHILE),
 	("else", Tokens.ELSE),
 	("if", Tokens.IF),
 	("return", Tokens.RETURN),
 	("end", Tokens.END),
 	("print", Tokens.PRINT)
 	])
 end