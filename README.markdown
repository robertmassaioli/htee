HTee
====

Welcome to the htee program written in Haskell. The purpose of this program is to be a cross
operating system implementation of the standard 'tee' program. There is no point continuously
implementing the same program for many different operating systems so I have decidet to implement it
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
