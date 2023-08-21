{-# LANGUAGE GADTs #-}
module Lex
    ( lexer
    , Token(..)
    )
    where
--------------------------------------------------------------------------------
import       Data.Char
import       Data.List

import AST
--------------------------------------------------------------------------------

lexer :: String -> [Token]

-- eof
lexer "" = []

-- discard whitespace
lexer (c:cs)
    | isSpace c  = lexer (dropWhile isSpace cs)

-- syntax
lexer (',':cs) = TokenComma     : lexer cs
lexer (';':cs) = TokenSemicolon : lexer cs
lexer ('(':cs) = TokenLParen    : lexer cs
lexer (')':cs) = TokenRParen    : lexer cs
lexer ('{':cs) = TokenLBrace    : lexer cs
lexer ('}':cs) = TokenRBrace    : lexer cs

-- operators
lexer ('=':'=':cs)  = TokenEqual     : lexer cs     
lexer ('!':'=':cs)  = TokenNotEqual  : lexer cs     
lexer ('!':cs)      = TokenNot       : lexer cs
lexer ('+':cs)      = TokenPlus      : lexer cs     
lexer ('-':cs)      = TokenMinus     : lexer cs     
lexer ('*':cs)      = TokenStar      : lexer cs     
lexer ('/':cs)      = TokenSlash     : lexer cs     

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

-- int literals
lexer s@(c:_)
    | isDigit c  = TokenNumber (toInt num) : lexer rest
    where
        (num, rest) = span isDigit s
        toInt = foldl (\n d -> 10 * n + digitToInt d) 0

