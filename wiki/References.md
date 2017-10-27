# References

In this page we collect online references to some of the topics of
interest to this course. Some of the material here are advanced topics
and are not part of the course per se. However, this page is meant to
be of interest if one wants to do further reading.

## Syntax analysis.

1. [Russ Cox's resource page][cox-regexp] on implementing regular
   expression matches. Among other things you can find the description
   and explanation of the
   [Ken Thompson's linear algorithm][ken-thompson-regexp] for regular
   expressions matching.

2. [Parser combinator][parser-combinator] libraries give another
   approach to parsing/lexing which is particularly well suited for
   functional programming language. The [parsec] library in Haskell is
   a important example which have clones in other languages as
   well. Advantage include not learning another language (lex/yacc)
   and better error message (particularly the Haskell variant).

## Programming language links

1. [The programming langauge zoo](http://plzoo.andrej.com/ "Programming language zoo")


[cox-regexp]: <https://swtch.com/~rsc/regexp/> "Russ Cox: Implementing regular expressions"
[ken-thompson-regexp]: <http://doi.acm.org/10.1145/363347.363387> "Ken Thompson's algorithm"
[parser-combinator]: <https://en.wikipedia.org/wiki/Parser_combinator> "Parser combinators"
[parsec]: <https://wiki.haskell.org/Parsec> "Parsec"
