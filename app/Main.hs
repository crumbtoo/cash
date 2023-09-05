module Main where

import Cash

main :: IO ()
main = do
    i <- getContents
    compileToTmp i

