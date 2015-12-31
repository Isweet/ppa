module PPA.Analysis.AvailableExpressions where

import qualified Data.Set as Set

import PPA.Lang.While
import PPA.Lang.While.Util

kill :: Stmt -> Lab -> Set.Set AExp
kill s l =
    case bf l of
        (BAssign x a _) -> Set.filter (\ a -> x `Set.member` fvA a) $ aExp s
        (BSkip _)       -> Set.empty
        (BBExp _ _)     -> Set.empty
    where
        bf :: Lab -> Block
        bf = blockMap $ blocks s

gen :: Stmt -> Lab -> Set.Set AExp
gen s l =
    case bf l of
        (BAssign x a _) -> Set.filter (\ a -> x `Set.notMember` fvA a) $ aExpA a
        (BSkip _)       -> Set.empty
        (BBExp b _)     -> aExpB b
    where
        bf :: Lab -> Block
        bf = blockMap $ blocks s
