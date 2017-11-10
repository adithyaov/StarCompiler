# README #

# A Compiler for Star that compiles to javascript.

### Content. 
The repository as of now (Nov 10 2017) has four branches

* master : contains the full project upto lexing without bad character recovery, parsing into an AST without the token positions being recorded, no JS code support
* alterstar: Initial branch now redundant (facing imminent removal).
* step check: Tesing out printing of AST and correcting parse error line number bug
* type chk and var def: Testing out type checking and variable definition


### How do I get set up? ###

* Use the CM make file "sources.cm" found in the respective branchs.
* All the branches are independent and can be used as is.
* The following libraries are used in make file:
  basis.cm
  ml-yacc-lib.cm
* After running "sources.cm", type "Star.parse("filename");" to compile the file corresponding to the filename.

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

* Adithya Kumar or Jude K Anil
