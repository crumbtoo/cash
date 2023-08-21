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

newtype ParserT i m o = ParserT { runParserT :: i -> m (i, o) }
type Parser i o = ParserT i Maybe o

evalParser :: Parser i o -> i -> Maybe o
evalParser p i = snd <$> runParserT p i

runParser :: Parser i o -> i -> Maybe (i, o)
runParser = runParserT

instance (Functor m) => Functor (ParserT i m) where
    fmap f (ParserT p) = ParserT $ \i -> fmap (mapSnd f) $ p i
        where mapSnd f (a, b) = (a, f b)

instance (Monad m) => Applicative (ParserT i m) where
    pure a = ParserT $ \i -> pure (i, a)
    
    ParserT m <*> ParserT k = ParserT $ \i -> do
        (i', f) <- m i
        (i'', a) <- k i'
        pure $ (i'', f a)

-- unhappy with the MonadPlus constraint. no way around it if ParserT is going
-- to be a transformer.
instance (MonadPlus m) => Alternative (ParserT i m) where
    empty = ParserT $ const empty

    ParserT m <|> ParserT k = ParserT $ \i -> m i <|> k i

instance (Monad m) => Monad (ParserT i m) where
    ParserT m >>= k = ParserT $ \i -> do
        (i', a) <- m i
        runParserT (k a) i'

instance (MonadPlus m) => MonadPlus (ParserT i m)

instance (MonadPlus m) => MonadFail (ParserT i m) where
    fail _ = mzero

--------------------------------------------------------------------------------

-- parse a single token satisfying a predicate
satisfy :: (Alternative m) => (a -> Bool) -> ParserT [a] m a
satisfy p = ParserT $ \case
        (x:xs) | p x -> pure (xs, x)
        _            -> empty

satisfies :: (MonadPlus m) => (a -> Bool) -> ParserT [a] m [a]
satisfies = many . satisfy

token :: (Alternative m, Eq a) => a -> ParserT [a] m a
token c = satisfy (==c)

lookahead :: (Alternative m) => ParserT [a] m a
lookahead = ParserT $ \case
        [] -> empty
        i  -> pure (i, head i)

anyToken :: (Alternative m) => ParserT [a] m a
anyToken = satisfy (const True)

string :: (Alternative m, Eq a) => [a] -> ParserT [a] m [a]
string s = ParserT $ \i ->
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
parselex p = evalParser p . lexer

-- parse :: String -> Maybe Expr
-- parse = evalParser atom . lexer

expr :: Parser [Token] Expr
expr = comparison

comparison :: Parser [Token] Expr
comparison = binopl Equal TokenEqual bsum
         <|> binopl NotEqual TokenNotEqual bsum

-- comparison = Equal    <$> (bsum <* token TokenEqual)    <*> bsum
--          <|> NotEqual <$> (bsum <* token TokenNotEqual) <*> bsum

bsum :: Parser [Token] Expr
bsum  = binopl Add TokenPlus bproduct
    <|> binopl Subtract TokenMinus bproduct

bproduct :: Parser [Token] Expr
bproduct = binopl Multiply TokenStar unary
       <|> binopl Divide TokenSlash unary

unary :: Parser [Token] Expr
unary = undefined

binopl :: (Eq i) => (a -> a -> b) -> i -> Parser [i] a -> Parser [i] b
binopl f t x = f <$> (x <* token t) <*> x

atom :: Parser [Token] Expr
atom  = Var <$> ident
    <|> Call <$> functionCall

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

