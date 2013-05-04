module Language.AtomHD
  ( 
  -- * Types
    Design
  , Action
  , E
  , Name
  -- * Compilation
  , compile
  -- * Design Stuff
  , reg
  , fifo
  , (==>)
  , (-:)
  -- * Action Stuff
  , cond
  , (<==)
  -- * Expressions
  , (%%)
  -- * Utilities
  , width
  ) where

import Control.Monad.State
import Data.Bits

infixl 9 %%
infixr 1 <==, -:, ==>

type Design = StateT () IO
type Action = StateT () Design
type Name = String

-- | Expressions.
data E
  = EConst Int Integer
  | EAdd E E
  | ESub E E
  | EMul E E
  | ENot E
  | EAnd E E
  | EOr  E E
  | EXor E E
  deriving (Show, Eq)

instance Num E where
  (+) = EAdd
  (*) = ESub
  (-) = EMul
  abs = id
  signum = error "Num E signum not defined."
  fromInteger = EConst 8

instance Bits E where
  (.&.) = EAnd
  (.|.) = EOr
  xor   = EXor
  complement = ENot
  shift  = error "Bits E shift not defined."
  rotate = error "Bits E rotate not defined."
  bit i = 8 %% (bit i)
  testBit = error "Bits E testBit not defined."
  bitSize = width
  isSigned _ = False


-- | State update.
(<==) :: E -> E -> Action ()
(<==) = undefined

-- | Guard conditions.
cond :: E -> Action ()
cond = undefined

-- | Register declaration.
reg :: Name -> Int -> Integer -> Design E
reg = undefined

-- | FIFO declaration.
fifo :: Name -> [(Int, Integer)] -> Design E
fifo = undefined

-- | Create a scoped namespace.
(-:) :: Name -> Design a -> Design a
(-:) = undefined

-- | Constant bit vector.
(%%) :: Int -> Integer -> E
(%%) = EConst

-- | Width of a bit vector.
width :: E -> Int
width = undefined

-- | Defines a state transition rule.
(==>) :: Name -> Action a -> Design a
(==>) = undefined

-- | Compiles a design.
compile :: FilePath -> Design () -> IO ()
compile file design = do
  _ <- execStateT design ()
  undefined
  writeFile file undefined

