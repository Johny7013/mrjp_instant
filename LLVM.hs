module LLVM (compile) where

import AbsInstant
import ErrM
import qualified Data.Map as M
import Control.Monad.State
import Data.List

-- function from runtime.ll
print_int :: String
print_int = unlines [
  "@dnl = internal constant [4 x i8] c\"%d\0A\00\"",
  "declare i32 @printf(i8*, ...)",
  "define void @printInt(i32 %x) {",
  "    %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0",
  "    call i32 (i8*, ...) @printf(i8* %t0, i32 %x)",
  "    ret void",
  "}"
  ]

main_def :: String
main_def = "define i32 @main(i32 %argc, i8** %argv) {\n"

prog_end :: String
prog_end = unlines [
  "    ret i32 0",
  "}"
  ]

runLLVMCompiler :: Program a -> String
runLLVMCompiler (Prog _ stmts) = "JOLO"


compile :: Program a -> IO()
compile tree =
  let compiled_tree = runLLVMCompiler tree
  in putStrLn $ unlines [print_int, main_def, compiled_tree, prog_end]
