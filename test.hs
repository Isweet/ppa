import While

printStmt :: Stmt -> IO ()
printStmt = putStrLn . showL

main :: IO ()
main = printStmt $ parse "y:=x; z:=1; while y>1 do (z:=z*y; y:=y-1); y:=0"
