module PPA.Lang.While.Util (Block (..), init, final, blocks, blockMap, labels, fv, fvA, fvB, flow, aExp, aExpA, aExpB) where

import Prelude hiding (init)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe

import PPA.Lang.While.Internal hiding (aExp)

data Block = BAssign Var AExp Lab | BSkip Lab | BBExp BExp Lab deriving (Eq, Ord)

instance Show Block where
    show (BAssign x a l) = "(" ++ (show l) ++ ") " ++ show (SAssign x a l)
    show (BSkip l)       = "(" ++ (show l) ++ ") " ++ show (SSkip l)
    show (BBExp b l)     = "(" ++ (show l) ++ ") " ++ (show b)

-- 2.1, p. 36
init :: Stmt -> Lab
init (SAssign _ _ l) = l
init (SSkip l)       = l
init (SSeq ss)       = init s_1
    where
        s_1 :: Stmt
        s_1 = head ss
init (SIf _ l _ _)   = l
init (SWhile _ l _)  = l

-- 2.1, p. 36
final :: Stmt -> Set.Set Lab
final (SAssign _ _ l) = Set.singleton l
final (SSkip l)       = Set.singleton l
final (SSeq ss)       = final s_n
    where
        s_n :: Stmt
        s_n = last ss
final (SIf _ _ s1 s2) = Set.unions [(final s1), (final s2)]
final (SWhile _ l _)  = Set.singleton l

-- 2.1, p. 36
blocks :: Stmt -> Set.Set Block
blocks (SAssign x a l) = Set.singleton $ BAssign x a l
blocks (SSkip l)       = Set.singleton $ BSkip l
blocks (SSeq ss)       = Set.unions bs
    where
        bs :: [Set.Set Block]
        bs = map blocks ss
blocks (SIf b l s1 s2) = Set.unions [Set.singleton $ BBExp b l, blocks s1, blocks s2]
blocks (SWhile b l s)  = Set.unions [Set.singleton $ BBExp b l, blocks s]

getLabel :: Block -> Lab
getLabel (BAssign _ _ l) = l
getLabel (BSkip l)       = l
getLabel (BBExp _ l)     = l

blockMap :: Set.Set Block -> Lab -> Block
blockMap bs l = Maybe.fromJust $ Map.lookup l bm
    where
        bm :: Map.Map Lab Block
        bm = Set.foldl (\ acc x -> Map.insert (getLabel x) x acc) Map.empty bs

-- 2.1, p. 37
labels :: Stmt -> Set.Set Lab
labels s = Set.map getLabel $ blocks s

-- 2.1, p. 37
flow :: Stmt -> Set.Set (Lab, Lab)
flow SAssign{} = Set.empty
flow (SSkip _)       = Set.empty
flow (SSeq ss)       = Set.unions [subflows, seqflows]
    where
        subflows :: Set.Set (Lab, Lab)
        subflows = Set.unions $ map flow ss

        seqflows :: Set.Set (Lab, Lab)
        seqflows = Set.unions $ mapPair flowPair ss
            where
                flowPair :: Stmt -> Stmt -> Set.Set (Lab, Lab)
                flowPair s1 s2 = Set.map (\ l -> (l, init s2)) $ final s1

                mapPair :: (Stmt -> Stmt -> Set.Set (Lab, Lab)) -> [Stmt] -> [Set.Set (Lab, Lab)]
                mapPair f ([x, y])         = [(f x y)]
                mapPair f (x:(tail@(y:_))) = (f x y):(mapPair f tail)
flow (SIf _ l s1 s2) = Set.unions [subflows, branchflows]
    where
        subflows :: Set.Set (Lab, Lab)
        subflows = Set.unions [flow s1, flow s2]

        branchflows :: Set.Set (Lab, Lab)
        branchflows = Set.unions [Set.singleton (l, init s1), Set.singleton (l, init s2)]
flow (SWhile _ l s) = Set.unions [subflows, bodyflows]
    where
        subflows :: Set.Set (Lab, Lab)
        subflows = flow s

        bodyflows :: Set.Set (Lab, Lab)
        bodyflows = Set.unions [Set.singleton (l, init s), Set.map (\ l' -> (l', l)) $ final s]

-- 2.1, p. 38
flowReverse :: Stmt -> Set.Set (Lab, Lab)
flowReverse s = Set.map (\ (l, l') -> (l', l)) $ flow s

fvA :: AExp -> Set.Set Var
fvA (AVar x)       = Set.singleton x
fvA (ANum _)       = Set.empty
fvA (AOpA _ a1 a2) = Set.unions [fvA a1, fvA a2]

fvB :: BExp -> Set.Set Var
fvB (BTrue)        = Set.empty
fvB (BFalse)       = Set.empty
fvB (BNot b)       = fvB b
fvB (BOpB _ b1 b2) = Set.unions [fvB b1, fvB b2]
fvB (BOpR _ a1 a2) = Set.unions [fvA a1, fvA a2]

-- 2.1, p. 38
-- NOTE: Referenced, but not defined
fv :: Stmt -> Set.Set Var
fv (SAssign x a _) = Set.unions [Set.singleton x, fvA a]
fv (SSkip _)       = Set.empty
fv (SSeq ss)       = Set.unions $ map fv ss
fv (SIf b _ s1 s2) = Set.unions [fvB b, fv s1, fv s2]
fv (SWhile b _ s)  = Set.unions [fvB b, fv s]

aExpA :: AExp -> Set.Set AExp
aExpA (AVar _) = Set.empty
aExpA (ANum _) = Set.empty
aExpA a@(AOpA _ a1 a2) = Set.unions [Set.singleton a, aExpA a1, aExpA a2]

aExpB :: BExp -> Set.Set AExp
aExpB (BTrue) = Set.empty
aExpB (BFalse) = Set.empty
aExpB (BNot b) = aExpB b
aExpB (BOpB _ b1 b2) = Set.unions [aExpB b1, aExpB b2]
aExpB (BOpR _ a1 a2) = Set.unions [aExpA a1, aExpA a2]

-- 2.1, p. 39
-- NOTE: Referenced, but not defined
aExp :: Stmt -> Set.Set AExp
aExp (SAssign _ a _) = aExpA a
aExp (SSkip _) = Set.empty
aExp (SSeq ss) = Set.unions $ map aExp ss
aExp (SIf b _ s1 s2) = Set.unions [aExpB b, aExp s1, aExp s2]
aExp (SWhile b _ s) = Set.unions [aExpB b, aExp s]


