import While

main :: IO ()
main = putStrLn $ showL $ testWhileParser "y:=x; z:=1; while y>1 do (z:=z*y; y:=y-1); y:=0"
