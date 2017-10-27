# Ideas for project.

The main idea of the project is to build a compiler. A compiler is a
moderately large project and some initial setup can make your task
easy. I suggest you read the [[Managing large projects]] page.  Break
up your project into two logical subparts namely the front end and the
backend.


## The front end of the compiler.

The front end of the compiler parses the input file and converts it
into the abstract syntax tree. Completing this includes.

1. Decide on the source language. You can choose a well known language
   like C, Ruby, python etc. However, it might make sense to stick to a
   simple subset of that language.

2. Having decided the language, you need to capture its abstract syntax
   as an ML data type.

3. We then define the parser and lexer for the file. Write your
   ml-yacc specification in a .grm file. Define a lexer for it in a
   .lex file. Design the yacc source in such a way that it builds the
   ast data type.

### Choosing a subset of the language

I would suggest building the compiler incrementally. A minimal
compiler to aim for is the following

1. Supports only the type int (all variables are int)
2. Supports expressions that are arithmetic operations on it.
3. Supports conditionals and loops.

This completes the front end of the compiler. When this part of the
project is complete, we will have an intermediate
evaluation.


Some things that you can leave out in the initial effort is

1. Function calls as implementing them requires understanding calling
   conventions and stack management.

2. Types like strings, arrays, records and general recursive data
   types.  This will require run time support and in particular
   garbage collection.

## The backend of the compiler.

The backend of the compiler converts the ast (captured by the data
type that you defined) to one of the target languages. A standard
target is the MIPS processor. The book gives an outline of this
approach. However you have many other options.

1. Target [LLVM]. Real life projects that do this include the clang C
   compiler, GHC haskell (the LLVM backend) etc.

2. Target the C programming language. Examples include the MLton
   compiler and stalin

3. Target JVM, the java virtual machine. Apart from the java language,
   real life projects like [scala], [clojure] etc.

4. Target [JavaScript] and run on the browser. Example of languages
   are [Elm], [PureScript] etc

5. Write your own code generator to an actual processor. I would
   suggest a risc processor like MIPS. For MIPS there is the SPIM
   simulator which you can use to test the program.

You will need to choose one of them and stick to it.

### Targeting LLVM

The [LLVM] backend is known to generate high quality code to various
targets. It also has a JIT mode which can be used for running things
in interpreted form. The disadvantage is the learning curve. For languages
like scheme, ruby etc, you would need to write a runtime for that language
and integrate it with LLVM


### Targeting C.

This is probably the easiest to start with because of knowledge of
C. C compilers like gcc and clang are known to produce high quality
executable. However, if your language is very different from C, then
you will need to add a lot of kludge.


### Targeting JVM/JavaScript

JVM has lot of interesting features that can make targeting languages
easy. In particular, since the memory is managed, writing a runtime
system can often be avoided. If you want your language to run on the
browser then Javascript is also an option but remember that javascript
is pretty quirky and it can lead to some untraceable bugs.

### Targeting MIPS

This exercise would give you a complete compiler which generates
machine instruction. You will also write code for register allocation
and other interesting stuff that you learn in this course.


# Some languages to write compilers for.

1. Scheme/Lisp
2. Ruby/Python
3. The Tiger language given in the book
4. Your own interesting example.

I would suggest that you do not go into designing a new language but
to take a subset of existing language so that you can get the lexer
and parser phase quickly.

# The reverse polish example.

The [compilers] repository contains a simple compiler that converts a
new line separated list of expressions into reverse polish
notation. The source is available at the
[reverse-polish directory][reverse-polish]. To connect it to the
context of writing a full compiler.

1. The datatype `Ast.Expr` (file [ast.sml]) is the abstract syntax of
   the expressions. The source language is a list of expressions.

2. The type `Machine.Program` (file [machine.sml]) is the target language.

3. The function `compile` in the file [translate.sml] is the code
   generator.  It takes a program in the source language (i.e
   `Ast.Expr list`) and compiles it into the target language
   (i.e. `Machine.Program`)

The source and target languages were so simple that we did not need an
intermediate language. Real examples would need something more
complex.

[reverse-polish]: <https://bitbucket.org/piyush-kurur/compilers/src/master/reverse-polish/>
[ast.sml]: <https://bitbucket.org/piyush-kurur/compilers/src/master/reverse-polish/ast.sml>
[machine.sml]: <https://bitbucket.org/piyush-kurur/compilers/src/master/reverse-polish/machine.sml>
[translate.sml]: <https://bitbucket.org/piyush-kurur/compilers/src/master/reverse-polish/translate.sml>
[compilers]: <https://bitbucket.org/piyush-kurur/compilers/>
[scala]:      <https://www.scala-lang.org/>
[clojure]:    <https://clojure.org/>
[purescript]: <http://www.purescript.org/>
[elm]:        <https://elm-lang.org>
[jvm]:        <https://en.wikipedia.org/wiki/Java_virtual_machine>
[llvm]:       <http://llvm.org/>
[javascript]: <https://www.javascript.com/>
[mlton]:      <http://www.mlton.org/>
[stalin]:     <https://github.com/barak/stalin>
