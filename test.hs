import While
import Text.Parsec

main :: IO ()
main = testWhileParser "y:=x; z:=1; while y>1 do (z:=z*y; y:=y-1); y:=0"
