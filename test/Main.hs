import PPA.While

import qualified Data.Set as Set
import qualified Data.List as List

printStmt :: Stmt -> IO ()
printStmt = putStrLn . showL

printSet :: (Show a) => Set.Set a -> IO ()
printSet s = putStrLn $ "{" ++ es ++ "}"
    where es = concat $ List.intersperse ", " $ map show $ Set.toList s

printCase :: String -> String -> IO ()
printCase name prog = do
    let s = parse prog
    putStr $ name ++ ": "
    printStmt s
    putStr $ "labels(" ++ name ++ ") = "
    printSet $ wlabels s
    putStr $ "init(" ++ name ++ ")   = "
    print $ winit s
    putStr $ "flow(" ++ name ++ ")   = "
    printSet $ wflow s
    putStr $ "final(" ++ name ++ ")  = "
    printSet $ wfinal s

power :: String
power = "z:=1; while x>0 do (z:=z*y; x:=x-1)"

factorial :: String
factorial = "y:=x; z:=1; while y>1 do (z:=z*y; y:=y-1); y:=0"

main :: IO ()
main = do
    printCase "power" power
    putStrLn ""
    printCase "factorial" factorial
