module PPA.Analysis.Util (Solution (..), defaultMust, defaultMay, fix) where

import Data.Set as Set

import PPA.Lang.While
import PPA.Lang.While.Util

-- Won't work, can't compare functions --> can't take the fixpoint of a solution
data Solution = Solution { entry :: Lab -> Set.Set AExp
                         , exit  :: Lab -> Set.Set AExp
                         }

defaultMust :: Stmt -> Solution
defaultMust s = Solution { entry = const aExpS
                         , exit  = const aExpS
                         }
    where
        aExpS :: Set.Set AExp
        aExpS = aExp s

defaultMay :: Stmt -> Solution
defaultMay s = Solution { entry = const Set.empty
                        , exit  = const Set.empty
                        }

fix :: (Eq a) => a -> (a -> a) -> a
fix start f = 
    if start == next then
        start
    else
        fix next f
    where
        next = f start
