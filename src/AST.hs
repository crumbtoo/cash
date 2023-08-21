{-# LANGUAGE OverloadedStrings, GADTs, DataKinds #-}
module AST
    ( AST(..)
    , Expr(..)
    , FunctionCall(..)
    , Stat(..)
    , Def(..)
    , Token(..)
    , writeAST

    , isIdent
    , isNumber
    )
    where
--------------------------------------------------------------------------------
import           Control.Monad.State
import           Data.GraphViz                          (printDotGraph)
import           Data.GraphViz.Types.Monadic
import           Data.GraphViz.Types.Generalised        (DotGraph)
import           Data.GraphViz.Attributes
import           Data.GraphViz.Attributes.Complete
import           Data.Text.Lazy                         (Text)
import qualified Data.Text.Lazy                         as T
import           Text.Printf
import           System.Process                         (system)
--------------------------------------------------------------------------------

data Token
    -- keywords
    = TokenFunction
    | TokenIf
    | TokenElse
    | TokenReturn
    | TokenLet
    | TokenWhile
    -- syntax
    | TokenComma
    | TokenSemicolon
    | TokenLParen
    | TokenRParen
    | TokenLBrace
    | TokenRBrace
    -- literals
    | TokenNumber Int
    | TokenIdent String
    -- exprs
    | TokenNot
    | TokenEqual
    | TokenNotEqual
    | TokenPlus
    | TokenStar
    | TokenMinus
    | TokenSlash
    deriving (Show, Eq)

isIdent :: Token -> Bool
isIdent (TokenIdent _) = True
isIdent _              = False

isNumber :: Token -> Bool
isNumber (TokenNumber _) = True
isNumber _              = False

--------------------------------------------------------------------------------

data Expr where
    Boolean     :: Bool -> Expr
    Number      :: Int -> Expr
    Ident       :: String -> Expr
    Not         :: Expr -> Expr
    Equal       :: Expr -> Expr -> Expr
    NotEqual    :: Expr -> Expr -> Expr
    Add         :: Expr -> Expr -> Expr
    Subtract    :: Expr -> Expr -> Expr
    Multiply    :: Expr -> Expr -> Expr
    Divide      :: Expr -> Expr -> Expr
    ExpCall     :: FunctionCall -> Expr

deriving instance Show Expr

data FunctionCall where
    FunctionCall :: String -> [Expr] -> FunctionCall

deriving instance Show FunctionCall

-- data Stat a = Call String [Expr a]
--             | Return (Expr a)
--             | Block [Stat a]
--             | If (Expr a) (Stat a) (Stat a)

data Block

data Stat where
    Call        :: String -> [Expr] -> Stat
    Return      :: Expr -> Stat
    Block       :: [Stat] -> Stat
    If          :: Expr -> Stat -> Stat -> Stat
    Let         :: String -> Expr -> Stat
    Assign      :: String -> Expr -> Stat
    While       :: Expr -> Stat -> Stat

-- data Def   = Function String [String]
data Def where
    FunctionDef    :: String -> [String] -> Stat -> Def

--------------------------------------------------------------------------------

type DotGen a = StateT Int (DotM Text) a

nodeg :: Attributes -> DotGen Text
nodeg attr = do
    k <- mklabel
    lift $ node k attr
    pure k

class AST a where
    dotAST :: a -> DotGen Text

label :: String -> Attribute
label = textLabel . T.pack

mklabel :: DotGen Text
mklabel = do
    n <- get
    modify succ
    pure $ "L" <> T.pack (show n)

writeAST :: (AST a) => a -> IO ()
writeAST ast = do
    writeFile "/tmp/t.dot" . T.unpack . printDotGraph $ do
        digraph (Str "AST") $ do
            nodeAttrs [shape Record]
            runStateT (dotAST ast) 0

    system "dot -Tsvg /tmp/t.dot > /tmp/t.svg"
    pure ()

--------------------------------------------------------------------------------

instance AST Expr where
    -- terminals
    dotAST (Number n) = nodeg [ label $ printf "{ Number | %d }" n ]
    dotAST (Ident k) = nodeg [ label $ printf "{ Ident | %s }" k ]

    -- non-terminals
    dotAST (Not e) = do
        k <- mklabel
        lift $ node k [ label "{ Not }" ]
        v <- dotAST e
        lift $ k --> v
        pure k
        

instance AST Stat where
    dotAST (Call f args) = undefined

