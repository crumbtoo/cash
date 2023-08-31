module Main where

import Cash

main :: IO ()
main = do
    i <- getContents
    printCompiler i

