module PPA.Analysis.AvailableExpressions where

import Prelude hiding (init)

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Array.IArray as Array

import PPA.Lang.While
import PPA.Lang.While.Util

type AE = Set.Set AExp

type VecAE = Array.Array Integer AE
type VecF = Array.Array Integer (Solution -> AE)

data Solution = Solution {  entryAE :: VecAE
                         ,  exitAE  :: VecAE
                         } deriving (Show, Eq)

data Solver = Solver {  entryF :: VecF
                     ,  exitF  :: VecF
                     }

genIter :: Integer -> Solver -> Solution -> Solution
genIter l s rd = Solution { entryAE = entries, exitAE = exits }
    where
        entries = Array.array (1, l) [ (i, ((entryF s) Array.! i) rd) | i <- [1..l]]
        exits = Array.array (1, l) [ (i, ((exitF s) Array.! i) rd) | i <- [1..l]]

-- compute fixpoint
fix :: (Eq a) => a -> (a -> a) -> a
fix start f =
    if start == next then
        start
    else
        fix next f
    where
        next = f start


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

ae :: Stmt -> Solution
ae s =
    fix start $ genIter sz solver
        where
        start = Solution { entryAE = upper, exitAE = upper }
            where
            upper :: VecAE
            upper = Array.array (1, sz) [(i, aExp s) | i <- [1..sz]]
        solver = Solver { entryF = aeEntry, exitF = aeExit }
            where
            aeEntry :: VecF
            aeEntry = Array.array (1, sz) [(i, (\ ae ->
                if i == init s then 
                    Set.empty 
                else
                    List.foldl Set.intersection (aExp s) [(exitAE ae) Array.! l' | (l', l) <- Set.toList $ flow s, l == i])) | i <- [1..sz]]
            aeExit :: VecF
            aeExit = Array.array (1, sz) [(i, (\ ae ->
                Set.union (((entryAE ae) Array.! i) Set.\\ (kill s i)) (gen s i))) | i <- [1..sz]]
        sz :: Integer 
        sz = toInteger $ Set.size $ labels s
