{-# LANGUAGE LambdaCase #-}
module Parse
    ( parse
    , evalParser

    , char
    , string
    , predicate
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

instance (Functor m) => Functor (ParserT i m) where
    fmap f (ParserT p) = ParserT $ \i -> fmap (mapSnd f) $ p i
        where mapSnd f (a, b) = (a, f b)

instance (Monad m) => Applicative (ParserT i m) where
    pure a = ParserT $ \i -> pure (i, a)
    
    ParserT m <*> ParserT k = ParserT $ \i -> do
        (i', f) <- m i
        (i'', a) <- k i'
        pure $ (i'', f a)

instance (MonadPlus m) => Alternative (ParserT i m) where
    empty = ParserT $ const mzero

    ParserT m <|> ParserT k = ParserT $ \i -> m i <|> k i

instance (Monad m) => Monad (ParserT i m) where
    ParserT m >>= k = ParserT $ \i -> do
        (i', a) <- m i
        runParserT (k a) i'

instance (MonadPlus m) => MonadPlus (ParserT i m)

instance (MonadPlus m) => MonadFail (ParserT i m) where
    fail _ = mzero

--------------------------------------------------------------------------------

predicate :: (Alternative m) => (a -> Bool) -> ParserT [a] m a
predicate p = ParserT $ \case
        (x:xs) | p x -> pure $ (xs, x)
        _            -> empty

char :: (Alternative m, Eq a) => a -> ParserT [a] m a
char c = predicate (==c)

string :: (Alternative m, Eq a) => [a] -> ParserT [a] m [a]
string s = ParserT $ \i ->
    case stripPrefix s i of
        (Just rest) -> pure (rest, s)
        _           -> empty

-- discord whitespace
ws :: (Alternative m) => ParserT String m ()
ws = ParserT $ \i -> pure (dropWhile isSpace i, ())

--------------------------------------------------------------------------------

parse :: Parser String [String]
parse = many (ws *> string "function" <* ws)

