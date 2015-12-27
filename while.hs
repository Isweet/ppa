{-# LANGUAGE FlexibleContexts #-}

module While (Stmt, showL, testWhileParser) where

import Control.Monad.Identity

import Prelude hiding (Num, GT, LT)
import Text.Parsec
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Char as Char
import qualified Text.ParserCombinators.Parsec.Language as Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Text.ParserCombinators.Parsec.Expr as Expr

type Var = String
type Num = Integer
type Lab = Integer

data OpA = Add | Subtract | Multiply | Divide

instance Show OpA where
    show Add = "+"
    show Subtract = "-"
    show Multiply = "*"
    show Divide = "/"

data OpB = And | Or

instance Show OpB where
    show And = "&&"
    show Or = "||"

data OpR = LT | GT

instance Show OpR where
    show LT = "<"
    show GT = ">"

data AExp = AVar Var | ANum Num | AOpA OpA AExp AExp

instance Show AExp where
    show (AVar x) = x
    show (ANum n) = show n
    show (AOpA o a1 a2) = "(" ++ (show a1) ++ " " ++ (show o) ++ " " ++ (show a2) ++ ")"

data BExp = BTrue | BFalse | BNot BExp | BOpB OpB BExp BExp | BOpR OpR AExp AExp

instance Show BExp where
    show BTrue = "true"
    show BFalse = "false"
    show (BNot b) = "~" ++ "(" ++ (show b) ++ ")"
    show (BOpB o b1 b2) = "(" ++ (show b1) ++ " " ++ (show o) ++ " " ++ (show b2) ++ ")"
    show (BOpR o a1 a2) = "(" ++ (show a1) ++ " " ++ (show o) ++ " " ++ (show a2) ++ ")"

data Stmt = SAssign Var AExp Lab | SSkip Lab | SSeq [Stmt] | SIf BExp Lab Stmt Stmt | SWhile BExp Lab Stmt

instance Show Stmt where
    show (SAssign x a _) = x ++ " := " ++ (show a) ++ ";"
    show (SSkip _) = "skip;"
    show (SSeq ss) = "(" ++ ((init . init) (concat (map (((flip (++)) " ") . show) ss))) ++ ");"
    show (SIf b _ s1 s2) = "if " ++ (show b) ++ " then " ++ (show s1) ++ " else " ++ (show s2)
    show (SWhile b _ s) = "while " ++ (show b) ++ " do " ++ (show s)

showL :: Stmt -> String
showL (SAssign x a l) = "[" ++ x ++ " := " ++ (show a) ++ "]^" ++ (show l) ++ ";"
showL (SSkip l) = "[" ++ "skip" ++ "]^" ++ (show l) ++ ";"
showL (SSeq ss) = "(" ++ ((init . init) (concat (map (((flip (++)) " ") . showL) ss))) ++ ");"
showL (SIf b l s1 s2) = "if " ++ "[" ++ (show b) ++ "]^" ++ (show l) ++ " then " ++ (showL s1) ++ " else " ++ (showL s2)
showL (SWhile b l s) = "while " ++ "[" ++ (show b) ++ "]^" ++ (show l) ++ " do " ++ (showL s)

-- Helpers
toSeq :: [Stmt] -> Stmt
toSeq (h:[]) = h
toSeq l = SSeq l

-- While Definition
whileDef = Language.emptyDef 
            {
                Token.commentStart  = "/*",
                Token.commentLine   = "//",
                Token.identStart    = Char.letter,
                Token.identLetter   = Char.alphaNum,
                Token.reservedNames = [ "if",
                                        "then",
                                        "else",
                                        "while",
                                        "do",
                                        "skip",
                                        "true",
                                        "false" ],
                Token.reservedOpNames = [ "+", 
                                          "-", 
                                          "*", 
                                          "/", 
                                          ":=",
                                          "<", 
                                          ">",
                                          "&&", 
                                          "||", 
                                          "~" ] 
            }

-- Lexer
whileLexer = Token.makeTokenParser whileDef

identifier = Token.identifier whileLexer
reserved = Token.reserved whileLexer
reservedOp = Token.reservedOp whileLexer
parens = Token.parens whileLexer

whiteSpace = Token.whiteSpace whileLexer
semi = Token.semi whileLexer
integer = Token.integer whileLexer

-- Parser

    -- Stmt
whileParser :: ParsecT String Integer Identity Stmt
whileParser = whiteSpace >> stmt

stmt :: ParsecT String Integer Identity Stmt
stmt =     parens stmt
       <|> seqStmt 

seqStmt :: ParsecT String Integer Identity Stmt
seqStmt = do
    stmts <- (sepBy1 stmt' semi)
    return $ toSeq stmts

stmt' :: ParsecT String Integer Identity Stmt
stmt' =     ifStmt
        <|> whileStmt
        <|> skipStmt
        <|> assignStmt

assignStmt :: ParsecT String Integer Identity Stmt
assignStmt = do
    var <- identifier
    reservedOp ":="
    exp <- aExp
    l <- getState
    modifyState (+1)
    return $ SAssign var exp l

skipStmt :: ParsecT String Integer Identity Stmt
skipStmt = do 
    reserved "skip"
    l <- getState
    modifyState (+1)
    return $ SSkip l

ifStmt :: ParsecT String Integer Identity Stmt
ifStmt = do 
    reserved "if"
    cond <- bExp
    l <- getState
    modifyState (+1)
    reserved "then"
    s1 <- stmt
    reserved "else"
    s2 <- stmt
    return $ SIf cond l s1 s2

whileStmt :: ParsecT String Integer Identity Stmt
whileStmt = do 
    reserved "while"
    cond <- bExp
    l <- getState
    modifyState (+1)
    reserved "do"
    s <- stmt
    return $ SWhile cond l s

    -- BExp
opB = [ [ Expr.Prefix (reservedOp "~"  >> return (BNot    ))                ],
        [ Expr.Infix  (reservedOp "&&" >> return (BOpB And)) Expr.AssocLeft,
          Expr.Infix  (reservedOp "||" >> return (BOpB Or )) Expr.AssocLeft ] ]

termB =     parens bExp
        <|> (reserved "true" >> return BTrue)
        <|> (reserved "false" >> return BFalse)
        <|> do
                a1 <- aExp
                op <- relation
                a2 <- aExp
                return $ BOpR op a1 a2
                    where
                        relation =     (reservedOp ">" >> return GT)
                                   <|> (reservedOp "<" >> return LT)

bExp :: ParsecT String Integer Identity BExp
bExp = Expr.buildExpressionParser opB termB

    -- AExp
opA = [ [ Expr.Infix (reservedOp "*" >> return (AOpA Multiply)) Expr.AssocLeft,
          Expr.Infix (reservedOp "/" >> return (AOpA Divide  )) Expr.AssocLeft ],
        [ Expr.Infix (reservedOp "+" >> return (AOpA Add     )) Expr.AssocLeft,
          Expr.Infix (reservedOp "-" >> return (AOpA Subtract)) Expr.AssocLeft ] ]

termA =     parens aExp
        <|> do
                var <- identifier
                return $ AVar var
        <|> do
                num <- integer
                return $ ANum num

aExp :: ParsecT String Integer Identity AExp
aExp = Expr.buildExpressionParser opA termA

-- Convenience

testWhileParser :: String -> Stmt
testWhileParser s = case runIdentity $ runParserT whileParser 1 "" s of
    Left l -> error $ show l
    Right r -> r
