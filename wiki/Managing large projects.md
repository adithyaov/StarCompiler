# Managing large projects.

This is probably the first course which involves a significant amount
of programming. In this page I add some suggestions on how to manage
such a project. Ideas here are general and can be used to maintain any
set of files.

## Getting started.

You _should_ use a version control right from the beginning and if
possible use one of the distributed version controls like [git]. I
will assume that you have chosen [git] for the rest of the
page. Ensure that


1. Make small and meaningful commits. These are easier to manage and
   review.

2. Have good log messages. If the change is trivial the title should
   explain what it is doing. Otherwise list out what the commit does.

3. Some irritating changes that gets committed when multiple users
   work on a repository is changes that add spaces at the end of the
   line or adds newlines at the end of the file. I usually enable the
   pre-commit hook in my git repository so that changes that contain
   these kind of lines are not permitted. Here is the simplest way to
   enable it.

	```
	mv .git/hooks/pre-commit.sample .git/hooks/pre-commit
	```
4. Consider hosting the git repository on one of the public hosting
   sites. If nothing else, this would act as a backup for your work in
   case your machine goes south.


## Your first commit.

Somethings are expected to be part of every project. This includes the
following.

1. A `README.md` file that gives a brief description of the projects
   and other stuff. This should be a text file and it makes sense to write it
   in say markdown.

2. A `.gitignore`. This file collects what files need to be ignored by
   [git]. If you are using some other version controller, there would
   be a similar file. The github repository [gitignore] contains a lot
   of sample `.gitignore` file depending on the language you are
   using. Using one of them would be a good idea.

3. A `LICENSE` file that specifies what is the licence under which you
   are releasing the source code. Some open source license worth
   considering is the [GPL] and [BSD3] licences. You can also release
   it to the public domain using something like [UNLICENSE].

This can be your first commit.

## Continuous integration.

A technique that improves the quality of code is to continuously build
every commit of your code. Ideally your code should have its own test
cases, but for a start, ensure that the code at least
builds. Continuous integration, CI for sort, means that every commit
that you make to a public repository is to be built and tested. Many
free continuous integration services are available for public
repositories. For bitbucket repositories like the [compilers]
repository, we use the [shippable].


[compilers]: <https://bitbucket.org/piyush-kurur/compilers/>
[shippable]: <https://www.shippable.com/>
[gitignore]: <https://github.com/github/gitignore>
[gpl]:       <https://www.gnu.org/licenses/gpl.html>
[bsd3]: <https://opensource.org/licenses/BSD-3-Clause>
[unlicense]: <http://unlicense.org/>
[git]: <https://git-scm.com/>
