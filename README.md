# Principles of Program Analysis (in Haskell)
## by Flemming Nielson, Hanne Riis Nielson, and Chris Hankin

### Summary

Implementations of analyses from Principles of Program Analysis by Nielson, Nielson, and Hankin.
The implementations are all written in Haskell, and example analyses are all parsed directly from the
book.

### Building

    % git clone git@github.com:Isweet/ppa.git
    % cd ppa
    % cabal sandbox init
    % cabal install --only-dependencies --enable-tests
    % cabal build
    % cabal test

### Parser

The parser for the While language (section 1.2, page 3) is written in Parsec. It is almost exactly
the parser presented in [https://wiki.haskell.org/Parsing\_a\_simple\_imperative\_language](https://wiki.haskell.org/Parsing_a_simple_imperative_language),
except that it includes functionality for generating a labeled AST.

Make sure you have a sandbox created (as per the build instructions above), then:

    % cabal repl
    *PPA.Lang.While.Syntax> :module +PPA.Lang.While.Util
    *PPA.Lang.While.Syntax PPA.Lang.While.Util> let factorial = "y:=x; z:=1; while y>1 do (z:=z*y; y:=y-1); y:=0"
    *PPA.Lang.While.Syntax PPA.Lang.While.Util> let s = read factorial :: Stmt
    *PPA.Lang.While.Syntax PPA.Lang.While.Util> showL s
    "[y := x]^1; [z := 1]^2; while [(y > 1)]^3 do ([z := (z * y)]^4; [y := (y - 1)]^5); [y := 0]^6"
    *PPA.Lang.While.Syntax PPA.Lang.While.Util> flow s
    fromList [(1,2),(2,3),(3,4),(3,6),(4,5),(5,3)]
