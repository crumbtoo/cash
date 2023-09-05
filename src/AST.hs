{-# LANGUAGE OverloadedStrings, GADTs, DataKinds #-}
module AST
    ( AST(..)
    , Expr(..)
    , Visibility(..)
    , FunctionCall(..)
    , Stat(..)
    , Token(..)

    , isNumber
    , isIdent

    , writeAST
    , writeAST'
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
    | TokenAssert
    | TokenPublic
    | TokenPrivate
    | TokenGoto
    -- syntax
    | TokenComma
    | TokenSemicolon
    | TokenColon
    | TokenLParen
    | TokenRParen
    | TokenLBrace
    | TokenRBrace
    -- terminals
    | TokenNumber Int
    | TokenIdent String
    -- exprs
    | TokenNot
    | TokenEqual
    | TokenNotEqual
    | TokenLT
    | TokenAssign
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
    LitNum      :: Int -> Expr
    Var         :: String -> Expr
    Not         :: Expr -> Expr
    Equal       :: Expr -> Expr -> Expr
    NotEqual    :: Expr -> Expr -> Expr
    CmpLT       :: Expr -> Expr -> Expr
    Add         :: Expr -> Expr -> Expr
    Subtract    :: Expr -> Expr -> Expr
    Multiply    :: Expr -> Expr -> Expr
    Divide      :: Expr -> Expr -> Expr
    Call        :: FunctionCall -> Expr

deriving instance Show Expr

type Ident = String

data FunctionCall = FunctionCall Ident [Expr]
    deriving (Show)

data Visibility = Public | Private
    deriving (Show, Eq)

data Stat where
    FunctionStat    :: Visibility -> Ident -> [Ident] -> Stat -> Stat
    ReturnStat      :: Expr -> Stat
    IfStat          :: Expr -> Stat -> Stat -> Stat
    WhileStat       :: Expr -> Stat -> Stat
    LabelStat       :: Ident -> Stat
    GotoStat        :: Ident -> Stat
    LetStat         :: String -> Expr -> Stat
    AssignStat      :: String -> Expr -> Stat
    BlockStat       :: [Stat] -> Stat
    ExprStat        :: Expr -> Stat
    CallStat        :: FunctionCall -> Stat
    AssertStat      :: Expr -> Stat

deriving instance Show Stat

--------------------------------------------------------------------------------

instance Semigroup Stat where
    a <> BlockStat []           = a
    BlockStat [] <> b           = b

    BlockStat a <> BlockStat b  = BlockStat $ a ++ b
    a           <> BlockStat b  = BlockStat $ a : b
    BlockStat a <> b            = BlockStat $ a ++ [b]
    a           <> b            = BlockStat $ [a,b]


instance Monoid Stat where
    mempty = BlockStat []

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

writeAST' :: (AST a) => Maybe a -> IO ()
writeAST' (Just a) = writeAST a
writeAST' Nothing = fail "parsing failed"

writeAST :: (AST a) => a -> IO ()
writeAST ast = do
    writeFile "/tmp/t.dot" . T.unpack . printDotGraph $ do
        digraph (Str "AST") $ do
            nodeAttrs [shape Record]
            runStateT (dotAST ast) 0

    system "dot -Tsvg /tmp/t.dot > /tmp/t.svg"
    pure ()

dotbin :: (AST a) => String -> a -> a -> DotGen Text
dotbin l a b = do
    p <- nodeg [ label l ]
    q <- dotAST a
    r <- dotAST b
    lift $ do
        p --> q
        p --> r
    pure p

dotunary :: (AST a) => String -> a -> DotGen Text
dotunary l a = do
    p <- nodeg [ label l ]
    q <- dotAST a
    lift $ p --> q
    pure p

addHint :: String -> DotGen Text -> DotGen Text
addHint l a = do
    p <- hint l
    q <- a
    lift $ p --> q
    pure p

hint :: String -> DotGen Text
hint l = nodeg [ label l, shape Ellipse ]

noder :: String -> DotGen Text
noder l = nodeg [ label l ]

--------------------------------------------------------------------------------

instance AST Expr where
    -- atoms
    dotAST (LitNum n) = nodeg [ label $ printf "{ LitNum | %d }" n ]
    dotAST (Var k) = nodeg [ label $ printf "{ Var | %s }" k ]

    -- operators
    dotAST (Not a) = dotunary "Not" a
    dotAST (Equal a b) = dotbin "Equal" a b
    dotAST (Add a b) = dotbin "Add" a b
    dotAST (Subtract a b) = dotbin "Subtract" a b
    dotAST (Multiply a b) = dotbin "Multiply" a b
    dotAST (Divide a b) = dotbin "Divide" a b

    dotAST (Call (FunctionCall f args)) = do
        p <- noder $ printf "{ FunctionCall | %s }" f
        q <- hint "args"
        r <- mapM dotAST args
        lift $ do
            p --> q
            mapM_ (q-->) r
        pure p
    

instance AST Stat where
    dotAST (FunctionStat vis name params body) = do
        p <- nodeg [ label $
                printf "{ FunctionStat | %s | %s | %s }"
                       (show vis)
                       name
                       (show params) ]
        q <- dotAST body
        lift $ p --> q
        pure q
    
    dotAST (BlockStat ss) = do
        p <- nodeg [ label "BlockStat" ]

        forM ss $ \s -> do
            q <- dotAST s
            lift $ p --> q

        pure p

    dotAST (ReturnStat e) = do
        p <- nodeg [ label "Return" ]
        q <- dotAST e
        lift $ p --> q
        pure p

    dotAST (IfStat c a b) = do
        p <- nodeg [ label "If" ]
        c' <- addHint "condition" $ dotAST c
        a' <- addHint "then" $ dotAST a
        b' <- addHint "else" $ dotAST b
        lift $ do
            p --> c'
            p --> a'
            p --> b'
        pure p

    dotAST (WhileStat c a) = do
        p <- noder "while"
        c' <- addHint "condition" $ dotAST a
        lift $ p --> c'
        pure p
        
          -- | WhileStat Expr Stat
          -- | VarStat Ident Expr
          -- | AssignStat Ident Expr
    dotAST (ExprStat e) = do
        p <- noder "ExprStat"
        q <- dotAST e
        lift $ p --> q
        pure p
          -- | CallStat FunctionCall
