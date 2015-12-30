module Main (main) where

import Prelude hiding (init)

import qualified Data.Set as Set
import qualified Data.List as List

import PPA.Lang.While.Internal.Syntax
import PPA.Lang.While.Util

-- Convenience

printStmt :: Stmt -> IO ()
printStmt = putStrLn . showL

printSet :: (Show a) => Set.Set a -> IO ()
printSet s = putStrLn $ "{" ++ es ++ "}"
    where es = concat $ List.intersperse ", " $ map show $ Set.toList s

printCase :: String -> String -> IO ()
printCase name prog = do
    let s = read prog
    putStr $ name ++ ": "
    printStmt s
    putStr $ "labels(" ++ name ++ ") = "
    printSet $ labels s
    putStr $ "init(" ++ name ++ ")   = "
    print $ init s
    putStr $ "flow(" ++ name ++ ")   = "
    printSet $ flow s
    putStr $ "final(" ++ name ++ ")  = "
    printSet $ final s

-- Progs

ifBasic :: String
ifBasic =
    "   if (a > 0 || a < 0) then \
      \     (x := -2)            \
      \ else                     \
      \     (skip)"

ifTwice :: String
ifTwice = 
    "   if (a > 0 || a < 0) then \ 
      \     (x := -2)            \ 
      \ else                     \ 
      \     (skip);              \ 
      \ if (b < 5) then          \
      \     (y := 1)             \
      \ else                     \
      \     (skip)"

{- y^x
 - result stored in z -}
power :: String
power = 
    "   z := 1;             \
      \ while (x > 0) do    \
      \     (z := z * y;    \
      \      x := x - 1)"

{- x!
 - result stored in z -}
factorial :: String
factorial = 
    "   y := x;             \
      \ z := 1;             \
      \ while (y > 1) do    \
      \     (z := z * y;    \
      \      y := y - 1);   \
      \ y := 0"

-- Harness

main :: IO ()
main = do
    printCase "power" power
    putStrLn ""
    printCase "factorial" factorial
    putStrLn ""
    printCase "ifBasic" ifBasic
    putStrLn ""
    printCase "ifTwice" ifTwice
