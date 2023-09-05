module Cash
    ( compiler
    , printCompiler
    , compileToTmp
    )
    where
--------------------------------------------------------------------------------
import           Control.Arrow              ((>>>))
import           Data.Function              ((&))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Foldable              (traverse_)
import           System.Process             (callCommand)

import Lex
import Parse
import CodeGen
import AST
import ARM
--------------------------------------------------------------------------------

compiler :: String -> Maybe Text
compiler = lexer
       >>> parser
       >>> fmap (traverse_ emit)
       >>> fmap (genASM [])

printCompiler :: String -> IO ()
printCompiler = compiler >>> traverse_ T.putStr

compileToTmp :: String -> IO ()
compileToTmp s = do
    let ipath = "/tmp/cash.s"
        opath = "/tmp/cash.out"

    compiler s & traverse_ (T.writeFile ipath)
    callCommand . unwords $
        ["arm-linux-gnueabihf-gcc", "-static", ipath, "-o", opath]

