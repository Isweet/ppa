{-# LANGUAGE FlexibleContexts #-}

module PPA.Lang.While.Internal where

import Prelude hiding (Num, GT, LT)

import Text.Parsec
import qualified Text.Parsec.Char as Char
import qualified Text.Parsec.Language as Language
import qualified Text.Parsec.Token as Token
import qualified Text.Parsec.Expr as Expr

import qualified Data.List as List

-- DATA
type Var = String
type Num = Integer
type Lab = Integer

data OpA = Add | Subtract | Multiply | Divide deriving (Eq, Ord)

data OpB = And | Or deriving (Eq, Ord)

data OpR = LT | GT deriving (Eq, Ord)

data AExp = AVar Var 
    | ANum Num 
    | AOpA OpA AExp AExp deriving (Eq, Ord)

data BExp = BTrue 
    | BFalse 
    | BNot BExp 
    | BOpB OpB BExp BExp 
    | BOpR OpR AExp AExp deriving (Eq, Ord)

data Stmt = SAssign Var AExp Lab 
    | SSkip Lab 
    | SSeq [Stmt] 
    | SIf BExp Lab Stmt Stmt 
    | SWhile BExp Lab Stmt deriving (Eq, Ord)

-- SHOW
instance Show OpA where
    show Add = "+"
    show Subtract = "-"
    show Multiply = "*"
    show Divide = "/"

instance Show OpB where
    show And = "&&"
    show Or = "||"

instance Show OpR where
    show LT = "<"
    show GT = ">"

instance Show AExp where
    show (AVar x) = x
    show (ANum n) = show n
    show (AOpA o a1 a2) = "(" ++ (show a1) ++ " " ++ (show o) ++ " " ++ (show a2) ++ ")"

instance Show BExp where
    show BTrue = "true"
    show BFalse = "false"
    show (BNot b) = "~" ++ "(" ++ (show b) ++ ")"
    show (BOpB o b1 b2) = "(" ++ (show b1) ++ " " ++ (show o) ++ " " ++ (show b2) ++ ")"
    show (BOpR o a1 a2) = "(" ++ (show a1) ++ " " ++ (show o) ++ " " ++ (show a2) ++ ")"

instance Show Stmt where
    show (SAssign x a _) = x ++ " := " ++ (show a)
    show (SSkip _) = "skip"
    show (SSeq ss) = (concat (List.intersperse "; " (map show ss)))
    show (SIf b _ s1 s2) = "if " ++ (show b) ++ " then (" ++ (show s1) ++ ") else (" ++ (show s2) ++ ")"
    show (SWhile b _ s) = "while " ++ (show b) ++ " do (" ++ (show s) ++ ")"

-- SHOW w/ Labels
showL :: Stmt -> String
showL (SAssign x a l) = "[" ++ x ++ " := " ++ (show a) ++ "]^" ++ (show l)
showL (SSkip l) = "[" ++ "skip" ++ "]^" ++ (show l)
showL (SSeq ss) = (concat (List.intersperse "; " (map showL ss)))
showL (SIf b l s1 s2) = "if " ++ "[" ++ (show b) ++ "]^" ++ (show l) ++ " then (" ++ (showL s1) ++ ") else (" ++ (showL s2) ++ ")"
showL (SWhile b l s) = "while " ++ "[" ++ (show b) ++ "]^" ++ (show l) ++ " do (" ++ (showL s) ++ ")"

-- READ
instance Read Stmt where
    readsPrec p s = [((tryParse s), "")]
        where
            tryParse :: String -> Stmt
            tryParse s = case runParser whileParser 1 "" s of
                Left l -> error $ show l
                Right r -> r

-- PARSING
type WhileParser = Parsec String Integer Stmt

-- While Definition
whileDef = Language.emptyDef 
            {
                Token.commentStart  = "/*",
                Token.commentEnd    = "*/",
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

whiteSpace = Token.whiteSpace whileLexer <?> "whitespace"
semi = Token.semi whileLexer
integer = Token.integer whileLexer

-- Parser
whileParser :: WhileParser
whileParser = do
    whiteSpace
    ret <- stmt
    eof
    return ret

stmt :: WhileParser
stmt =      parens stmt
        <|> seqStmt
        <?> "statement"

seqStmt :: WhileParser
seqStmt = do
    stmts <- (sepBy1 stmt' semi)
    return $ toSeq stmts
    where
        toSeq :: [Stmt] -> Stmt
        toSeq (h:[]) = h
        toSeq l = SSeq l

stmt' :: WhileParser
stmt' =     ifStmt
        <|> whileStmt
        <|> skipStmt
        <|> assignStmt

assignStmt :: WhileParser
assignStmt = do
    var <- identifier
    reservedOp ":="
    exp <- aExp
    l <- getState
    modifyState (+1)
    return $ SAssign var exp l
    <?> "assignment"

skipStmt :: WhileParser
skipStmt = do 
    reserved "skip"
    l <- getState
    modifyState (+1)
    return $ SSkip l
    <?> "skip"

ifStmt :: WhileParser
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
    <?> "if"

whileStmt :: WhileParser
whileStmt = do 
    reserved "while"
    cond <- bExp
    l <- getState
    modifyState (+1)
    reserved "do"
    s <- stmt
    return $ SWhile cond l s
    <?> "while"

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

bExp :: Parsec String Integer BExp
bExp = Expr.buildExpressionParser opB termB <?> "boolean expression"

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

aExp :: Parsec String Integer AExp
aExp = Expr.buildExpressionParser opA termA <?> "arithmetic expression"
