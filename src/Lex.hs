{-# LANGUAGE GADTs #-}
module Lex
    ( lexer
    )
    where
--------------------------------------------------------------------------------
import       Data.Char
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
    | TokenInt Int
    | TokenIdent String
    -- exprs
    | TokenNot
    | TokenEqual
    | TokenNotEqual
    | TokenPlus
    | TokenStar
    | TokenMinus
    | TokenSlash
    deriving Show
    
lexer :: String -> [Token]

-- eof
lexer "" = []

-- syntax
lexer (',':cs) = TokenComma     : lexer cs
lexer (';':cs) = TokenSemicolon : lexer cs
lexer ('(':cs) = TokenLParen    : lexer cs
lexer (')':cs) = TokenRParen    : lexer cs
lexer ('{':cs) = TokenLBrace    : lexer cs
lexer ('}':cs) = TokenRBrace    : lexer cs

-- keywords
lexer s
    | w == "function"  = TokenFunction    : lexer rest
    | w == "if"        = TokenIf          : lexer rest
    | w == "else"      = TokenElse        : lexer rest
    | w == "return"    = TokenReturn      : lexer rest
    | w == "let"       = TokenLet         : lexer rest
    | w == "while"     = TokenWhile       : lexer rest
    where (w,rest) = span isLetter s

-- identifier
lexer (c:cs)
    | isNameHead c  = TokenIdent (c : nameTail) : lexer rest
    where
        isNameHead = isLetter |.| (== '_')
        (nameTail, rest) = span (isLetter |.| isDigit |.| (=='_')) cs

        f |.| g = \a -> f a || g a

