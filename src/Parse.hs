{-# LANGUAGE LambdaCase #-}
module Parse
    ( parse
    , runParserT
    , runParser
    , evalParser

    , char
    , string
    , satisfies

    , ws
    , comment
    , cws

    -- re-exports
    , (<|>)
    , some
    , many
    )
    where
--------------------------------------------------------------------------------
import       Control.Applicative
import       Control.Monad
import       Data.List
import       Data.Char

import AST
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

satisfies :: (Alternative m) => (a -> Bool) -> ParserT [a] m a
satisfies p = ParserT $ \case
        (x:xs) | p x -> pure $ (xs, x)
        _            -> empty

allThat :: (MonadPlus m) => (a -> Bool) -> ParserT [a] m [a]
allThat p = many (satisfies p)

char :: (Alternative m, Eq a) => a -> ParserT [a] m a
char c = satisfies (==c)

string :: (Alternative m, Eq a) => [a] -> ParserT [a] m [a]
string s = ParserT $ \i ->
    case stripPrefix s i of
        (Just rest) -> pure (rest, s)
        _           -> empty

-- discord whitespace
ws :: (MonadPlus m) => ParserT String m ()
ws = void $ some (satisfies isSpace)

comment :: (MonadPlus m) => ParserT String m String
comment = string "//" *> allThat (/='\n') <* optional (char '\n')

cws :: (MonadPlus m) => ParserT String m ()
cws = void $ many (ws <|> void comment)

--------------------------------------------------------------------------------

parse :: Parser String [String]
parse = many (ws *> string "function" <* ws)

