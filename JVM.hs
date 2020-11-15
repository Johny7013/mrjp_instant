module JVM (compile) where

import AbsInstant
import ErrM
import qualified Data.Map as M
import Control.Monad.Reader
import Control.Monad.State

compile :: Program a -> IO()
compile prog = putStrLn "JOLO"
