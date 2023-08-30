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
import       Control.Monad.Zip
import       Data.List
import       Data.Functor

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

instance (Monad m) => MonadZip (ParserT i m) where
    mzip = liftA2 (,)

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

-- shitty hack because i'm a shit programmer
-- flipAssoc :: Expr -> Expr
-- flipAssoc (Add a (Add b c)) = Add (Add (flipAssoc a) (flipAssoc b)) (flipAssoc c)

parselex :: Parser [Token] o -> String -> Maybe o
parselex p = evalParser p . lexer

binfix :: Parser i (o -> o -> o) -> Parser i o -> Parser i o
binfix opp termp = foldl (\acc (f,b) -> acc `f` b) <$> termp <*> opterms
    where opterms = many (opp `mzip` termp)

expr :: Parser [Token] Expr
expr = comparison

comparison :: Parser [Token] Expr
comparison = binfix opp bsum
    where opp = token TokenEqual $> Equal
            <|> token TokenNotEqual $> NotEqual

bsum :: Parser [Token] Expr
bsum = binfix opp bproduct
    where opp = token TokenPlus $> Add
            <|> token TokenMinus $> Subtract

bproduct :: Parser [Token] Expr
bproduct = binfix opp unary
    where opp = token TokenStar $> Multiply
            <|> token TokenSlash $> Divide

unary :: Parser [Token] Expr
unary = Not <$> (token TokenNot *> atom)
    <|> atom

atom :: Parser [Token] Expr
atom  = Call <$> functionCall
    <|> Var <$> ident
    <|> LitNum <$> number
    <|> parenthesised expr

functionCall :: Parser [Token] FunctionCall
functionCall = FunctionCall <$>
    ident <*> (token TokenLParen *> argList <* token TokenRParen)
    where
        argList :: Parser [Token] [Expr]
        argList = commaSeparated expr

commaSeparated :: Parser [Token] o -> Parser [Token] [o]
commaSeparated p = (:) <$> p <*> (many $ token TokenComma *> p)
               <|> pure []

ident :: Parser [Token] String
ident = fmap (\(TokenIdent s) -> s) $ satisfy isIdent

number :: Parser [Token] Int
number = fmap (\(TokenNumber n) -> n) $ satisfy isNumber

parenthesised :: Parser [Token] o -> Parser [Token] o
parenthesised p = token TokenLParen *> p <* token TokenRParen

--------------------------------------------------------------------------------

stat :: Parser [Token] Stat
stat  = stat' <* token TokenSemicolon
    <|> functionStat -- no semicolon
    where stat' = assignStat
              <|> exprStat
              <|> ifStat
              <|> whileStat
              <|> letStat
              <|> returnStat
              <|> blockStat

returnStat :: Parser [Token] Stat
returnStat = ReturnStat <$> (token TokenReturn *> expr)

exprStat :: Parser [Token] Stat
exprStat = ExprStat <$> expr

ifStat :: Parser [Token] Stat
ifStat = IfStat
    <$> (token TokenIf *> parenthesised expr)
    <*> stat
    <*> ((token TokenElse *> stat) <|> pure (BlockStat []))

whileStat :: Parser [Token] Stat
whileStat = WhileStat
    <$> expr
    <*> stat

letStat :: Parser [Token] Stat
letStat = LetStat
    <$> (token TokenLet *> ident)
    <*> (token TokenAssign *> expr)

assignStat :: Parser [Token] Stat
assignStat = AssignStat
    <$> ident
    <*> (token TokenAssign *> expr)

blockStat :: Parser [Token] Stat
blockStat = BlockStat <$> (token TokenLBrace *> many stat <* token TokenRBrace)

functionStat :: Parser [Token] Stat
functionStat = FunctionStat
    <$> (token TokenFunction *> ident)
    <*> parenthesised parameters
    <*> blockStat
    where parameters = commaSeparated ident

