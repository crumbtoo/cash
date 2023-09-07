module Main where

import Ruelang

main :: IO ()
main = do
    i <- getContents
    compileToTmp i

