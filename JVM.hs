module JVM (compile) where

import AbsInstant
import ErrM
import qualified Data.Map as M
import Control.Monad.Trans.RWS
import Control.Monad (forM_, when)
import Data.List (unlines)

type Loc = Int
type CompilerState = (Loc, M.Map Ident Loc)
type CompilerResult = [String]
type CompilerMonad = RWS () CompilerResult CompilerState

--
data Instruction =
  Store Loc |
  Load Loc |
  PushConst Integer |
  PrintInt |
  GetStream |
  Add |
  Sub |
  Mul |
  Div

predefined_indexes :: [Int]
predefined_indexes = [0, 1, 2, 3]
-- emitIns :: String -> Loc -> Val -> Val -> String
-- emitIns op l v1 v2 = show (L l) ++ " = " ++ op ++ " i32 " ++ show v1 ++ ", " ++ show v2

instance Show Instruction where
  show (Store l)
    | l `elem` predefined_indexes = "istore_" ++ show l
    | otherwise = "istore " ++ show l
  show (Load l)
    | l `elem` predefined_indexes = "iload_" ++ show l
    | otherwise = "iload " ++ show l
  show (PushConst i)
    | i <= 5 = "iconst_" ++ show i
    | i <= 127 = "bipush " ++ show i
    | i <= 32767 = "sipush " ++ show i
    | otherwise = "ldc " ++ show i
  show PrintInt = "invokevirtual java/io/PrintStream/println(I)V"
  show GetStream = "getstatic java/lang/System/out Ljava/io/PrintStream;"
  show Add = "iadd"
  show Sub = "isub"
  show Mul = "imul"
  show Div = "idiv"
--

emitStmts :: [Stmt a] -> CompilerMonad ()
emitStmts stmts = do
  count_locals stmts
  optimized_stmts <- count_stack_depth stmts
  forM_ optimized_stmts emitStmt

count_locals :: [Stmt a] -> CompilerMonad ()
count_locals stmts = do
  forM_ stmts add_local
  (loc, env) <- get
  tell [limit_locals ++ show ((M.size env) + 1)]

add_local :: Stmt a -> CompilerMonad ()
add_local (SAss _ ident _) = do
  (loc, env) <- get
  when (M.notMember ident env) $ put (loc + 1, M.insert ident loc env)
add_local (SExp _ expr) = do
  return ()

count_stack_depth :: [Stmt a] -> CompilerMonad [Stmt a]
count_stack_depth stmts =
  let (optimized_stmts, stack_depths) = unzip $ map count_stmt_depth stmts
  in do
    tell [limit_stack ++ show (maximum stack_depths)]
    return optimized_stmts

-- change name
count_stmt_depth :: Stmt a -> (Stmt a, Int)
count_stmt_depth (SAss a ident expr) =
  let (new_expr, depth) = count_expr_depth expr
  in (SAss a ident new_expr, depth)
count_stmt_depth (SExp a expr) =
  let (new_expr, depth) = count_expr_depth expr
  in (SExp a new_expr, depth + 1) -- to check if 1 is neccesary

-- remove those expr at the beginning of the argument
count_expr_depth :: Exp a -> (Exp a, Int)
count_expr_depth (ExpAdd a expr_left expr_right) = count_bin_expr_depth_commutative expr_left expr_right (ExpAdd a)
count_expr_depth (ExpSub a expr_left expr_right) = count_bin_expr_depth_noncommutative expr_left expr_right (ExpSub a)
count_expr_depth (ExpMul a expr_left expr_right) = count_bin_expr_depth_commutative expr_left expr_right (ExpMul a)
count_expr_depth (ExpDiv a expr_left expr_right) = count_bin_expr_depth_noncommutative expr_left expr_right (ExpDiv a)
count_expr_depth expr@(ExpLit _ _) = (expr, 1)
count_expr_depth expr@(ExpVar _ _) = (expr, 1)

count_bin_expr_depth_commutative :: Exp a -> Exp a -> (Exp a -> Exp a -> Exp a) -> (Exp a, Int)
count_bin_expr_depth_commutative expr_left expr_right expr_raw =
  let (new_expr_left, d1) = count_expr_depth expr_left
      (new_expr_right, d2) = count_expr_depth expr_right
  in if d1 == d2 then (expr_raw new_expr_left new_expr_right, d1 + 1)
     else if d1 > d2 then (expr_raw new_expr_left new_expr_right, d1) else (expr_raw new_expr_right new_expr_left, d2)

count_bin_expr_depth_noncommutative :: Exp a -> Exp a -> (Exp a -> Exp a -> Exp a) -> (Exp a, Int)
count_bin_expr_depth_noncommutative expr_left expr_right expr_raw =
  let (new_expr_left, d1) = count_expr_depth expr_left
      (new_expr_right, d2) = count_expr_depth expr_right
  in (expr_raw new_expr_left new_expr_right, max d1 (d2 + 1))

emitStmt :: Stmt a -> CompilerMonad ()
emitStmt (SAss _ ident expr) = do
  emitExpr expr
  (_, env) <- get
  tell [show (Store $ env M.! ident)]
emitStmt (SExp _ expr) = do
  tell [show GetStream]
  emitExpr expr
  tell [show PrintInt]

emitBinExpr :: Instruction -> Exp a -> Exp a -> CompilerMonad ()
emitBinExpr insCon exp1 exp2 = do
  emitExpr exp1
  emitExpr exp2
  tell [show insCon]

emitExpr :: Exp a -> CompilerMonad ()
emitExpr (ExpAdd _ expr1 expr2) = emitBinExpr Add expr1 expr2
emitExpr (ExpSub _ expr1 expr2) = emitBinExpr Sub expr1 expr2
emitExpr (ExpMul _ expr1 expr2) = emitBinExpr Mul expr1 expr2
emitExpr (ExpDiv _ expr1 expr2) = emitBinExpr Div expr1 expr2
emitExpr (ExpLit _ int) = do
  tell [show (PushConst int)]
emitExpr (ExpVar _ ident) = do
  (_, env) <- get
  tell [show $ Load $ env M.! ident]



prog_beggining :: String
prog_beggining = unlines [
  ".class public Instant",
  ".super java/lang/Object",
  "",
  "; standard initializer",
  ".method public <init>()V",
  tab ++ "aload_0",
  tab ++ "invokespecial java/lang/Object/<init>()V",
  tab ++ "return",
  ".end method",
  "",
  "",
  ".method public static main([Ljava/lang/String;)V"
  ]

prog_end :: String
prog_end = unlines [
  "    return",
  ".end method"
  ]

tab :: String
tab = "    "

limit_locals :: String
limit_locals = ".limit locals "

limit_stack :: String
limit_stack = ".limit stack "

addTabsBeginning :: [String] -> [String]
addTabsBeginning l = map (tab ++) l

compileStmts :: [Stmt a] -> CompilerResult
compileStmts stmts =
  let (_, _, res) = runRWS (emitStmts stmts) () (1, M.empty)
  in res

runJVMCompiler :: Program a -> String
runJVMCompiler (Prog _ stmts) =
  let result = compileStmts stmts
  in unlines $ addTabsBeginning result

compile :: Program a -> IO()
compile tree =
  let compiled_tree = runJVMCompiler tree
  in putStrLn $ unlines [prog_beggining, compiled_tree, prog_end]
