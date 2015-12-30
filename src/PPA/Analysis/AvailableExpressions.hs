module PPA.Analysis.AvailableExpressions where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import PPA.Lang.While
import PPA.Lang.While.Util

kill :: Stmt -> Block -> Set.Set AExp
kill s (BAssign x a _) = Set.filter (\ a -> x `Set.member` fvA a) $ aExp s
kill s (BSkip _)       = Set.empty
kill s (BBExp _ _)     = Set.empty

gen :: Stmt -> Block -> Set.Set AExp
gen s (BAssign x a _) = Set.filter (\ a -> x `Set.notMember` fvA a) $ aExpA a
gen s (BSkip _)       = Set.empty
gen s (BBExp b _)     = aExpB b

toRes :: Stmt -> (Stmt -> Block -> Set.Set AExp) -> Map.Map Lab (Set.Set AExp)
toRes s f = Set.foldl (\ acc x -> Map.insert (getLabel x) (f s x) acc) Map.empty $ blocks s

killRes :: Stmt -> Map.Map Lab (Set.Set AExp)
killRes s = toRes s kill

genRes :: Stmt -> Map.Map Lab (Set.Set AExp)
genRes s = toRes s gen
