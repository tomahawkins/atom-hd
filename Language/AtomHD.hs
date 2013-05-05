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
  , local
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
  , locals :: [(Int, E)]
  , rules  :: [([Name], E, [(E, E)])]
  }

data ActionDB = ActionDB
  { guard   :: E
  , updates :: [(E, E)]
  }

-- | Expressions.
data E
  = EConst Int Integer
  | EReg   Int Int  -- id, width
  | ELocal Int Int  -- id, width
  | EAdd   E E
  | ESub   E E
  | EMul   E E
  | ENot   E
  | EAnd   E E
  | EOr    E E
  | EXor   E E
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
a <== b = modify $ \ d -> d { updates = updates d ++ [(a, b)] }

-- | Guard conditions.
cond :: E -> Action ()
cond a = modify $ \ d -> d { guard = EAnd (guard d) a }

-- | Register declaration.
reg :: Name -> Int -> Integer -> Design E
reg name width value = do
  d <- get
  put d { nextId = nextId d + 1, regs = regs d ++ [(nextId d, (path d ++ [name], width, value))] }
  return $ EReg (nextId d) width

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

-- | Create a local expression.
local :: E -> Design E
local a = do
  d <- get
  put d { nextId = nextId d + 1,  locals = locals d ++ [(nextId d, a)] }
  return $ ELocal (nextId d) (width a)

-- | Width of a bit vector.
width :: E -> Int
width a = case a of
  EConst w _ -> w
  EReg   _ w -> w
  ELocal _ w -> w
  EAdd   a _ -> width a
  ESub   a _ -> width a
  EMul   a _ -> width a
  ENot   a   -> width a
  EAnd   a _ -> width a
  EOr    a _ -> width a
  EXor   a _ -> width a

-- | Defines a state transition rule.
(==>) :: Name -> Action () -> Design ()
name ==> rule = do
  ActionDB guard updates <- execStateT rule ActionDB { guard = false, updates = [] }
  modify $ \ d -> d { rules = rules d ++ [(path d ++ [name], guard, updates)] }

-- | Compiles a design.
compile :: FilePath -> Design () -> IO ()
compile file design = do
  DesignDB _ _ regs locals rules <- execStateT design DesignDB { nextId = 0, path = [], regs = [], locals = [], rules = [] }
  putStrLn "Registers:"
  mapM_ print regs
  putStrLn "Local expressions:"
  mapM_ print locals
  putStrLn "Rules:"
  mapM_ print rules
  writeFile file "" --XXX

false :: E
false = 1%0

true :: E
true = 1%1

