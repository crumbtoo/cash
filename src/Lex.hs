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
lexer (':':cs) = TokenColon     : lexer cs
lexer ('(':cs) = TokenLParen    : lexer cs
lexer (')':cs) = TokenRParen    : lexer cs
lexer ('{':cs) = TokenLBrace    : lexer cs
lexer ('}':cs) = TokenRBrace    : lexer cs

-- operators
lexer ('=':'=':cs)  = TokenEqual     : lexer cs     
lexer ('!':'=':cs)  = TokenNotEqual  : lexer cs     
lexer ('<':cs)      = TokenLT        : lexer cs     
lexer ('=':cs)      = TokenAssign    : lexer cs     
lexer ('!':cs)      = TokenNot       : lexer cs
lexer ('+':cs)      = TokenPlus      : lexer cs     
lexer ('-':cs)      = TokenMinus     : lexer cs     
lexer ('*':cs)      = TokenStar      : lexer cs     
lexer ('/':cs)      = TokenSlash     : lexer cs     
lexer ('&':cs)      = TokenAmpersand : lexer cs     

-- keywords
lexer s
    | w == "fn"        = TokenFunction    : lexer rest
    | w == "function"  = TokenFunction    : lexer rest
    | w == "if"        = TokenIf          : lexer rest
    | w == "else"      = TokenElse        : lexer rest
    | w == "return"    = TokenReturn      : lexer rest
    | w == "let"       = TokenLet         : lexer rest
    | w == "while"     = TokenWhile       : lexer rest
    | w == "assert"    = TokenAssert      : lexer rest
    | w == "pub"       = TokenPublic      : lexer rest
    | w == "public"    = TokenPublic      : lexer rest
    | w == "private"   = TokenPrivate     : lexer rest
    | w == "goto"      = TokenGoto        : lexer rest
    where (w,rest) = span isLetter s

-- identifier
lexer (c:cs)
    | isNameHead c  = TokenIdent (c : nameTail) : lexer rest
    where
        isNameHead = isLetter |.| (== '_')
        (nameTail, rest) = span (isLetter |.| isDigit |.| (=='_')) cs

        f |.| g = \a -> f a || g a

lexer ('\'':c:'\'':cs) = TokenNumber (ord c) : lexer cs

-- int literals
lexer s@(c:_)
    -- todo hex
    -- todo char
    | isDigit c   = TokenNumber (foldDigits 10 num) : lexer rest
    where
        (num, rest) = span isDigit s
        foldDigits base = foldl (\n d -> base * n + digitToInt d) 0

