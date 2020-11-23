module Main where


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)
import System.FilePath.Posix (takeBaseName)

import LexInstant
import ParInstant
import SkelInstant
import PrintInstant
import AbsInstant

import qualified JVM
import qualified LLVM


import ErrM

type ParseFun a = [Token] -> Err (Program a)

myLLexer = myLexer

type Verbosity = Int

data Lang = JVM | LLVM | Empty

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Show a) => Verbosity -> ParseFun a -> Lang -> FilePath -> IO ()
runFile v p l f = readFile f >>= run v p l (takeBaseName f)

run :: (Show a) => Verbosity -> ParseFun a -> Lang -> String -> String -> IO ()
run v p l baseName s = let ts = myLLexer s in case p ts of
            Bad s    -> do putStrLn "\nParse              Failed...\n"
                           putStrV v "Tokens:"
                           putStrV v $ show ts
                           putStrLn s
                           exitFailure
            Ok  tree -> do
                           case l of
                             JVM -> JVM.compile tree baseName
                             otherwise -> LLVM.compile tree

                           exitSuccess


showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  -jvm (file)     Complie jvm."
    , "  -llvm (file)    Complie llvm"
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    "-jvm":fs -> mapM_ (runFile 2 pProgram JVM) fs
    "-llvm":fs -> mapM_ (runFile 2 pProgram LLVM) fs
    otherwise -> exitFailure
