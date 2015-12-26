module While (Stmt, BExp, AExp, OpR, OpB, OpA, Num, Var, whileParser) where 

import Prelude hiding (Num, GT, LT)
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Char as Char
import qualified Text.ParserCombinators.Parsec.Language as Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Text.ParserCombinators.Parsec.Expr as Expr

type Var = String
type Num = Integer

data OpA = Add | Subtract | Multiply | Divide deriving (Show)
data OpB = And | Or deriving (Show)
data OpR = LT | GT deriving (Show)

data AExp = AVar Var | ANum Num | AOpA OpA AExp AExp deriving (Show)
data BExp = BTrue | BFalse | BNot BExp | BOpB OpB BExp BExp | BOpR OpR AExp AExp deriving (Show)
data Stmt = SAssign Var AExp | SSkip | SSeq [Stmt] | SIf BExp Stmt Stmt | SWhile BExp Stmt deriving (Show)

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
whileParser :: Parser Stmt
whileParser = whiteSpace >> stmt

stmt :: Parser Stmt
stmt =     parens stmt
       <|> seqStmt 

seqStmt :: Parser Stmt
seqStmt = do
    stmts <- (sepBy1 stmt' semi)
    return $ toSeq stmts

stmt' :: Parser Stmt
stmt' =     ifStmt
        <|> whileStmt
        <|> skipStmt
        <|> assignStmt

assignStmt :: Parser Stmt
assignStmt = do
    var <- identifier
    reservedOp ":="
    exp <- aExp
    return $ SAssign var exp

skipStmt :: Parser Stmt
skipStmt = do 
    reserved "skip"
    return $ SSkip

ifStmt :: Parser Stmt
ifStmt = do 
    reserved "if"
    cond <- bExp
    reserved "then"
    s1 <- stmt
    reserved "else"
    s2 <- stmt
    return $ SIf cond s1 s2

whileStmt :: Parser Stmt
whileStmt = do 
    reserved "while"
    cond <- bExp
    reserved "do"
    s <- stmt
    return $ SWhile cond s

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

bExp :: Parser BExp
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

aExp :: Parser AExp
aExp = Expr.buildExpressionParser opA termA
