module LLVM (compile) where

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

instance Show Val where
  show (I i) = show i
  show (L l) = "%r" ++ show l

data Instruction =
  StoreVal Loc Val |
  PrintInt Val |
  Add Loc Val Val |
  Sub Loc Val Val |
  Mul Loc Val Val |
  Div Loc Val Val

emitIns :: String -> Loc -> Val -> Val -> String
emitIns op l v1 v2 = show (L l) ++ " = " ++ op ++ " i32 " ++ show v1 ++ ", " ++ show v2

instance Show Instruction where
  show (StoreVal l x) = show (L l) ++ " = add i32 0, " ++ show x
  show (PrintInt l) = "call void @printInt(i32 " ++ show l ++ ")"
  show (Add l x y) = emitIns "add" l x y
  show (Sub l x y) = emitIns "sub" l x y
  show (Mul l x y) = emitIns "mul" l x y
  show (Div l x y) = emitIns "sdiv" l x y

emitStmts :: [Stmt a] -> CompilerMonad ()
emitStmts stmts = forM_ stmts emitStmt

emitStmt :: Stmt a -> CompilerMonad ()
emitStmt (SAss _ ident expr) = do
  l <- emitExpr expr
  (loc, env) <- get
  new_loc <- getNewLoc
  put (loc + 1, M.insert ident new_loc env)
  tell [show (StoreVal new_loc l)]
emitStmt (SExp _ expr) = do
  l <- emitExpr expr
  tell [show (PrintInt l)]

type BinInstructionConstructor = Loc -> Val -> Val -> Instruction

emitBinExpr :: BinInstructionConstructor -> Exp a -> Exp a -> CompilerMonad Val
emitBinExpr insCon exp1 exp2 = do
  v1 <- emitExpr exp1
  v2 <- emitExpr exp2
  new_loc <- getNewLoc
  tell [show (insCon new_loc v1 v2)]
  return (L new_loc)


emitExpr :: Exp a -> CompilerMonad Val
emitExpr (ExpAdd _ exp1 exp2) = emitBinExpr Add exp1 exp2
emitExpr (ExpSub _ exp1 exp2) = emitBinExpr Sub exp1 exp2
emitExpr (ExpMul _ exp1 exp2) = emitBinExpr Mul exp1 exp2
emitExpr (ExpDiv _ exp1 exp2) = emitBinExpr Div exp1 exp2
emitExpr (ExpLit _ int) = do
  return $ I int
emitExpr (ExpVar _ ident) = do
  (_, env) <- get
  return $ L $ env M.! ident


getNewLoc :: CompilerMonad Loc
getNewLoc = do
  (l, env) <- get
  put (l + 1, env)
  return $ l + 1

-- function from runtime.ll
print_int :: String
print_int = unlines [
  "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"",
  "declare i32 @printf(i8*, ...)",
  "define void @printInt(i32 %x) {",
  tab ++ "%t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0",
  tab ++ "call i32 (i8*, ...) @printf(i8* %t0, i32 %x)",
  tab ++ "ret void",
  "}"
  ]

main_def :: String
main_def = "define i32 @main(i32 %argc, i8** %argv) {\n"

prog_end :: String
prog_end = unlines [
  tab ++ "ret i32 0",
  "}"
  ]

compileStmts :: [Stmt a] -> CompilerResult
compileStmts stmts =
  let (_, _, res) = runRWS (emitStmts stmts) () (-1, M.empty)
  in res

tab :: String
tab = "    "

addTabsBeginning :: [String] -> [String]
addTabsBeginning l = map (tab ++) l

runLLVMCompiler :: Program a -> String
runLLVMCompiler (Prog _ stmts) =
  let result = compileStmts stmts
  in unlines $ addTabsBeginning result


compile :: Program a -> IO()
compile tree =
  let compiled_tree = runLLVMCompiler tree
  in putStrLn $ unlines [print_int, main_def, compiled_tree, prog_end]
