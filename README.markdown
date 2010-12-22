HTee
====

Welcome to the htee program written in Haskell. The purpose of this program is to be a cross
operating system implementation of the standard 'tee' program. There is no point continuously
implementing the same program for many different operating systems so I have decided to implement it
once and release it under a license that is free for both commercial and non-commercial use. The aim
is to help the lives of the developers. We should have our basic tools work in expected ways no
matter what operating system we are on. We should not have to choose bettween one operating system
or the other because one has tools we need and the other does not (at least not for what I consider
to be the basics) and tee is certainly one of the basics.

Installation Instructions
-------------------------

Install it just like a normal haskell program. Download the source and then just:

    cabal configure && cabal build && cabal install

And make sure that your cabal bin directory is in your environments PATH variable. That is all that
there is to it.

Command Line Options
--------------------

You can get these by simply asking for help:

    $ htee --help
    Usage: htee [options] output_files...
      -V, -v  --version  print program version number
      -H, -h  --help     print this help message
      -a      --append   append to the files, do not truncate them

And that is everything that you can currently do with htee.

Author: _Robert Massaioli_

A big thanks to everyone that made a comment on the [/r/haskell post][1] that I made.

 [1]: http://www.reddit.com/r/haskell/comments/epmhk/reimplemented_tee_in_haskell_in_a_few_lines_for/
