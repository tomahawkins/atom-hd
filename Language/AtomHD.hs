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
  , (%)
  , true
  , false
  -- * Utilities
  , width
  ) where

import Control.Monad.State hiding (guard)
import Data.Bits

infixl 9 %
infixr 1 <==, -:, ==>

type Design = StateT DesignDB IO
type Action = StateT ActionDB Design
type Name = String

data DesignDB = DesignDB
  { nextId :: Int
  , path   :: [Name]
  , regs   :: [(Int, ([Name], Int, Integer))]
  , rules  :: [([Name], E, [(E, E)])]
  }

data ActionDB = ActionDB
  { guard   :: E
  , updates :: [(E, E)]
  }

-- | Expressions.
data E
  = EConst Int Integer
  | EReg Int
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
  bit i = 8 % (bit i)
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
reg name width value = do
  d <- get
  put d { nextId = nextId d + 1, regs = regs d ++ [(nextId d, (path d ++ [name], width, value))] }
  return $ EReg $ nextId d

-- | FIFO declaration.
fifo :: Name -> [(Int, Integer)] -> Design E
fifo = undefined

-- | Create a scoped namespace.
(-:) :: Name -> Design a -> Design a
name -: design = do
  modify $ \ d -> d { path = path d ++ [name] }
  a <- design
  modify $ \ d -> d { path = init $ path d }
  return a

-- | Constant bit vector.
(%) :: Int -> Integer -> E
(%) = EConst

-- | Width of a bit vector.
width :: E -> Int
width = undefined

-- | Defines a state transition rule.
(==>) :: Name -> Action () -> Design ()
name ==> rule = do
  ActionDB guard updates <- execStateT rule ActionDB { guard = false, updates = [] }
  modify $ \ d -> d { rules = rules d ++ [(path d ++ [name], guard, updates)] }

-- | Compiles a design.
compile :: FilePath -> Design () -> IO ()
compile file design = do
  _ <- execStateT design DesignDB { nextId = 0, path = [], regs = [], rules = [] }
  undefined
  writeFile file undefined

false :: E
false = 1%0

true :: E
true = 1%1

