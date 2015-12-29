module PPA.Lang.While.Util (Block, init, final, blocks, labels, flow) where

import Prelude hiding (init)

import qualified Data.Set as Set

import PPA.Lang.While.Internal.Syntax

data Block = BAssign Var AExp Lab | BSkip Lab | BBExp BExp Lab deriving (Eq, Ord)

instance Show Block where
    show (BAssign x a l) = "(" ++ (show l) ++ ") " ++ (show $ SAssign x a l)
    show (BSkip l) = "(" ++ (show l) ++ ") " ++ (show $ SSkip l)
    show (BBExp b l) = "(" ++ (show l) ++ ") " ++ (show b)

init :: Stmt -> Lab
init (SAssign _ _ l) = l
init (SSkip l) = l
init (SSeq ss) = init s_1
    where
        s_1 :: Stmt
        s_1 = head ss
init (SIf _ l _ _) = l
init (SWhile _ l _) = l

final :: Stmt -> Set.Set Lab
final (SAssign _ _ l) = Set.singleton l
final (SSkip l) = Set.singleton l
final (SSeq ss) = final s_n
    where
        s_n :: Stmt
        s_n = last ss
final (SIf _ _ s1 s2) = Set.unions [(final s1), (final s2)]
final (SWhile _ l _) = Set.singleton l

blocks :: Stmt -> Set.Set Block
blocks (SAssign x a l) = Set.singleton $ BAssign x a l
blocks (SSkip l) = Set.singleton $ BSkip l
blocks (SSeq ss) = Set.unions $ bs
    where
        bs :: [Set.Set Block]
        bs = map blocks ss
blocks (SIf b l s1 s2) = Set.unions [Set.singleton $ BBExp b l, blocks s1, blocks s2]
blocks (SWhile b l s) = Set.unions [Set.singleton $ BBExp b l, blocks s]

labels :: Stmt -> Set.Set Lab
labels s = Set.map getLabel $ blocks s
    where 
        getLabel (BAssign _ _ l) = l
        getLabel (BSkip l) = l
        getLabel (BBExp _ l) = l

flow :: Stmt -> Set.Set (Lab, Lab)
flow (SAssign _ _ _) = Set.empty
flow (SSkip _) = Set.empty
flow (SSeq ss) = Set.unions [subflows, seqflows]
    where
        subflows :: Set.Set (Lab, Lab)
        subflows = Set.unions $ map flow ss

        seqflows :: Set.Set (Lab, Lab)
        seqflows = Set.unions $ mapPair flowPair ss
            where
                flowPair :: Stmt -> Stmt -> Set.Set (Lab, Lab)
                flowPair s1 s2 = Set.map (\ l -> (l, init s2)) $ final s1

                mapPair :: (Stmt -> Stmt -> Set.Set (Lab, Lab)) -> [Stmt] -> [Set.Set (Lab, Lab)]
                mapPair f (x:y:[]) = (f x y):[]
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


