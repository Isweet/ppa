import While

printStmt :: Stmt -> IO ()
printStmt = putStrLn . showL

power :: String
power = "z:=1; while x>0 do (z:=z*y; x:=x-1)"

main :: IO ()
main = do
    let s = parse power
    print $ winit s
    print $ wfinal s
    print $ wblocks s
    print $ wlabels s
    print $ wflow s
