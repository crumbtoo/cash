{-# LANGUAGE LambdaCase #-}
module Parse
    -- ( parse
    -- , runParserT
    -- , runParser
    -- , evalParser

    -- , token
    -- , anyToken
    -- , string
    -- , satisfy
    -- , satisfies

    -- -- , ws
    -- -- , comment
    -- -- , cws

    -- -- re-exports
    -- , (<|>)
    -- , some
    -- , many
    -- )
    where
--------------------------------------------------------------------------------
import       Control.Applicative
import       Control.Monad
import       Data.List

import AST
import Lex (lexer)
--------------------------------------------------------------------------------

newtype ParserT i m o = ParserT { runParserT :: Int -> i -> m (i, o) }
type Parser i o = ParserT i Maybe o

evalParser :: Parser i o -> Int -> i -> Maybe o
evalParser p pr i = snd <$> runParserT p pr i

runParser :: Parser i o -> Int -> i -> Maybe (i, o)
runParser = runParserT

instance (Functor m) => Functor (ParserT i m) where
    fmap f (ParserT p) = ParserT $ \pr i -> fmap (mapSnd f) $ p pr i
        where mapSnd f (a, b) = (a, f b)

instance (Monad m) => Applicative (ParserT i m) where
    pure a = ParserT . const $ \i -> pure (i, a)
    
    ParserT m <*> ParserT k = ParserT $ \pr i -> do
        (i', f) <- m pr i
        (i'', a) <- k pr i'
        pure $ (i'', f a)

-- unhappy with the MonadPlus constraint. no way around it if ParserT is going
-- to be a transformer.
instance (MonadPlus m) => Alternative (ParserT i m) where
    empty = ParserT . const $ const empty

    ParserT m <|> ParserT k = ParserT $ \pr i -> m pr i <|> k pr i

instance (Monad m) => Monad (ParserT i m) where
    ParserT m >>= k = ParserT $ \pr i -> do
        (i', a) <- m pr i
        runParserT (k a) pr i'

instance (MonadPlus m) => MonadPlus (ParserT i m)

instance (MonadPlus m) => MonadFail (ParserT i m) where
    fail _ = mzero

--------------------------------------------------------------------------------
 
-- assign a given parser a precedence, making a parser that fails if ran with a
-- lower precedence than specified
withPrec :: (MonadPlus m) => Int -> ParserT i m o -> ParserT i m o
withPrec p' pa = ParserT $ \p i -> guard (p <= p') *> runParserT pa p' i

-- run parser with the next precedence level
higher :: Parser i o -> Parser i o
higher pa = ParserT $ \p i -> runParserT pa (succ p) i

-- parse a single token satisfying a predicate
satisfy :: (Alternative m) => (a -> Bool) -> ParserT [a] m a
satisfy p = ParserT . const $ \case
        (x:xs) | p x -> pure (xs, x)
        _            -> empty

satisfies :: (MonadPlus m) => (a -> Bool) -> ParserT [a] m [a]
satisfies = many . satisfy

token :: (Alternative m, Eq a) => a -> ParserT [a] m a
token c = satisfy (==c)

lookahead :: (Alternative m) => ParserT [a] m a
lookahead = ParserT . const $ \case
        [] -> empty
        i  -> pure (i, head i)

anyToken :: (Alternative m) => ParserT [a] m a
anyToken = satisfy (const True)

string :: (Alternative m, Eq a) => [a] -> ParserT [a] m [a]
string s = ParserT $ \pr i ->
    case stripPrefix s i of
        (Just rest) -> pure (rest, s)
        _           -> empty

-- discord whitespace
-- ws :: (MonadPlus m) => ParserT String m ()
-- ws = void $ some (satisfy isSpace)

-- comment :: (MonadPlus m) => ParserT String m String
-- comment   = string "//" *> satisfies (/='\n') <* optional (char '\n')
--         <|> string "/*" *> (satisfies (/='*') <* string "*/")

-- cws :: (MonadPlus m) => ParserT String m ()
-- cws = void $ many (ws <|> void comment)

--------------------------------------------------------------------------------

parselex :: Parser [Token] o -> String -> Maybe o
parselex p = evalParser p 0 . lexer

-- parse :: String -> Maybe Expr
-- parse = evalParser atom . lexer

expr :: Parser [Token] Expr
expr  = comparison
    <|> bsum
    <|> bproduct
    <|> unary
    <|> atom

comparison :: Parser [Token] Expr
comparison = binopl 3 Equal TokenEqual
         <|> binopl 3 NotEqual TokenNotEqual

bsum :: Parser [Token] Expr
bsum  = binopl 6 Add TokenPlus
    <|> binopl 6 Subtract TokenMinus

bproduct :: Parser [Token] Expr
bproduct = binopl 7 Multiply TokenStar
       <|> binopl 7 Divide TokenSlash

atom :: Parser [Token] Expr
atom  = Call <$> functionCall 
    <|> LitNum <$> number
    <|> Var <$> ident

unary :: Parser [Token] Expr
unary = Not <$> (token TokenNot *> atom)

binopl :: Int -> (Expr -> Expr -> Expr) -> Token -> Parser [Token] Expr
binopl pr f t = withPrec pr $ f <$> (higher expr <* token t) <*> higher expr

-- atom :: Parser [Token] Expr
-- atom  = Call <$> functionCall
--     <|> Var <$> ident
--     <|> LitNum <$> number
--     <|> token TokenLParen *> expr <* token TokenRParen

functionCall :: Parser [Token] FunctionCall
functionCall = FunctionCall <$>
    ident <*> (token TokenLParen *> argList <* token TokenRParen)

argList :: Parser [Token] [Expr]
argList   = (:) <$> expr <*> (many $ token TokenComma *> expr)
        <|> pure []

ident :: Parser [Token] String
ident = fmap (\(TokenIdent s) -> s) $ satisfy isIdent

number :: Parser [Token] Int
number = fmap (\(TokenNumber n) -> n) $ satisfy isNumber

