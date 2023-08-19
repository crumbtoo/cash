{-# LANGUAGE OverloadedStrings, GADTs, DataKinds #-}
module AST
    ( AST(..)
    , Expr(..)
    , FunctionCall(..)
    , Stat(..)
    , Def(..)
    , writeAST
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

-- data AST
--     -- terminals
--     = Number Int
--     | Ident String
--     -- expr
--     | Not AST -- TODO: this should be an expression, not any AST
--     | Equal AST AST
--     | NotEqual AST AST
--     | Add AST AST
--     | Subtract AST AST
--     | Multiply AST AST
--     | Divide AST AST
--     | Call String [AST]
--     deriving (Eq, Ord, Show)

-- data Token = TokenNumber Int
--            | TokenIdent String

-- data Expr = Number Int
--           | Ident String
--           | Not Expr
--           | Equal Expr Expr
--           | NotEqual Expr Expr
--           | Add Expr Expr
--           | Subtract Expr Expr
--           | Multiply Expr Expr
--           | Divide Expr Expr

data Expr a where
    Boolean     :: Bool -> Expr Bool
    Number      :: Int -> Expr Int
    Ident       :: String -> Expr String
    Not         :: Expr Bool -> Expr Bool
    Equal       :: Expr a -> Expr b -> Expr Bool
    NotEqual    :: Expr a -> Expr b -> Expr Bool
    Add         :: Expr Int -> Expr Int -> Expr Int
    Subtract    :: Expr Int -> Expr Int -> Expr Int
    Multiply    :: Expr Int -> Expr Int -> Expr Int
    Divide      :: Expr Int -> Expr Int -> Expr Int
    ExpCall     :: FunctionCall -> Expr a

deriving instance Show (Expr a)

data FunctionCall where
    FunctionCall :: String -> [Expr a] -> FunctionCall

deriving instance Show FunctionCall

-- data Stat a = Call String [Expr a]
--             | Return (Expr a)
--             | Block [Stat a]
--             | If (Expr a) (Stat a) (Stat a)

data Block

data Stat a where
    Call        :: String -> [Expr a] -> Stat a
    Return      :: Expr a -> Stat a
    Block       :: [Stat a] -> Stat Block
    If          :: Expr Bool -> Stat a -> Stat b -> Stat c
    Let         :: String -> Expr a -> Stat a
    Assign      :: String -> Expr a -> Stat a
    While       :: Expr a -> Stat Block -> Stat b

-- data Def   = Function String [String]
data Def a where
    Function    :: String -> [String] -> Stat Block -> Def a

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

instance AST (Expr a) where
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
        

instance AST (Stat a) where
    dotAST (Call f args) = undefined

