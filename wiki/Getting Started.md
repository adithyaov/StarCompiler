# Getting Started.

I will try to follow the book
[Modern Compiler Construction in ML][modern] by
[Andrew W. Appel][appel]. Since this course uses ML as the language to
build the compiler, having access to an ML language environment is a
must. The tools we would need for this course are

- The SML/NJ distribution.

- ML-Yacc

- ML-Lex

- ML-burg

- mlton optimizing compiler.

The lab machines in the institute will have these tools
installed. However, you might like to install these on your laptops. I
recommend a modern GNU/Linux distribution for your developmental needs
for this course. All these tools are available for free and most
GNU/Linux distros or their BSD cousins should have prebuild packages
for them. On a [Debian] based system the following commands will
install all these tools for you.


```
sudo apt update
sudo apt install smlnj smlnj-doc ml-yacc ml-lex ml-burg
sudo apt install mlton
```

Some Debian derivatives do not seem to install the `libsmlnj-smlnj`
(See #4) which has a lot of data structures useful for this course. Install
that as well.

```
sudo apt install libsmlnj-smlnj

```

As part of this course you are expected to do a semester long project
which involves building a non-trivial compiler. When building large
projects, it makes sense to use version control tools like git.


```
sudo apt install git gitk tig
sudo apt install magit        # magit mode for emacs users
sudo apt install vim-fugitive # interaction mode for vim users.
```

You may also consider hosting your project on sites like bitbucket or
github.

## Assignments, labsessions, updates

All material releavant for this course will be updated in the course
repository https://bitbucket.org/piyush-kurur/compilers/. An easy way
to keep track of the updates is to register on bitbucket and "watch"
this repository.

## Problems, Queries, Suggestions

Feel free to edit this wiki. Also make good use of the issue tracker
for this course. Not only does it help in future iterations of this
course, it also gives you a feel in participating in a modern open
source project.

[modern]: <https://www.cs.princeton.edu/~appel/modern/ml/>
[appel]: <https://www.cs.princeton.edu/~appel>
[debian]: <https://www.debian.org>
