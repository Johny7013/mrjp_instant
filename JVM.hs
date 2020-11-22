module JVM (compile) where

import AbsInstant
import ErrM
import qualified Data.Map as M
import Control.Monad.Trans.RWS
import Control.Monad (forM_)
import Data.List (unlines)

type Loc = Int
data Val = I Integer | L Loc
type CompilerState = (Loc, M.Map Ident Loc)
type CompilerResult = [String]
type CompilerMonad = RWS () CompilerResult CompilerState



tab :: String
tab = "    "

addTabsBeginning :: [String] -> [String]
addTabsBeginning l = map (tab ++) l

runJVMCompiler :: Program a -> String
runLLVMCompiler (Prog _ stmts) =
  let result = compileStmts stmts
  in unlines $ addTabsBeginning result


compile :: Program a -> IO()
compile tree =
  let compiled_tree = runJVMCompiler tree
  in putStrLn $ unlines [print_int, main_def, compiled_tree, prog_end]
