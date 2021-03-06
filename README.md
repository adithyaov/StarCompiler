# README #

# A Compiler for Star that compiles to javascript.

### Content. 
The repository as of now (Nov 10 2017) has two branches

* master : contains the full project upto lexing without bad character recovery, parsing into an AST without the token positions being recorded, no JS code support
* type chk and var def: Testing out type checking and variable definition


### How do I get set up? ###

* Use the CM make file "sources.cm" found in the respective branchs.
* All the branches are independent and can be used as is.
* The following libraries are used in make file:
  basis.cm, ml-yacc-lib.cm
* Star is the main stricture where all the functions are.
* Star.makeJS (filename: string, ast: StarAst.Prog\_) basically makes the JS file.
* Star.parse (inputFile: string) basically parses the input file and generates an (SOME ast).
* Star.showLog() shows the log after parsing.
* Star.startJS (ast: StarAst.Prog\_) can be used to view the result in the interpreter.
* Star.prefix is a mutable var to change the prefix of the code generated.

### Example ###

\- val x = Star.parse("infile.star");  
\- val y = valOf x;  
\- val Star.startJS y; (\* Shows the result in the console \*)  
\- val Star.makeJS ("out.js", y); (\* Outputs a js file \*)  
\- Star.showLog(); (\* Shows the log, ie. Errors, Def, Types etc. \*)  
\- Star.prefix := "the\_prefix\_"; (\* Changes the prefix. \*)  


### common files ###
* star.lex contains the ml-lex specification file for Star language
* star.grm contains the ml-yacc specification file for star language
* starAst.sml contains the datatypes for the ast.
* star.sml contains the glue code for lexer and parser, accepting input source file, code generation of javascript code for the created ast.
* other files are mostly ml-lex or ml-yacc generated files.

### What is this repository for? ###

* This repository is used in developing a compiler that compiles Star to javascript.
* (https://bitbucket.org/tutorials/markdowndemo)


### sml resources

* http://cs.fit.edu/~ryan/sml/intro.html
* http://www.cs.princeton.edu/courses/archive/fall08/cos441/notes/lect-SMLNJ.pdf
* http://www.cs.cornell.edu/riccardo/prog-smlnj/notes-011001.pdf

### lex sources
* http://smlnj.org/doc/ML-Lex/manual.html
* http://flint.cs.yale.edu/cs421/lectureNotes/c02.pdf
* http://www.cs.tufts.edu/comp/181/ug.pdf


### Who do I talk to? ###

* Adithya Kumar O V    - 111501017@smail.iitpkd.ac.in 
* Jude K Anil          - 111501011@smail.iitpkd.ac.in
