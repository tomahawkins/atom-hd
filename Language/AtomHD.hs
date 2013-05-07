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
  -- ** Interface Methods
  , methodValue
  , methodAction
  , methodActionValue
  -- * Action Stuff
  , cond
  , (<==)
  , display
  , finish
  -- * Expressions
  , local
  , (%)
  , true
  , false
  , module Data.Monoid
  , (#)
  , (##)
  , (==.)
  , (/=.)
  , mux
  -- * Utilities
  , width
  , bytes
  , bits
  ) where

import Control.Monad.State hiding (guard)
import Data.Bits
import Data.List
import Data.Monoid
import Text.Printf

infixl 9 %, #, ## -- , !
infix  4 ==., /=.
infixr 1 <==, -:, ==>

type Design = StateT DesignDB IO
type Action = StateT ActionDB Design
type Name = String

data DesignDB = DesignDB
  { path    :: [Name]
  , regs    :: [([Name], (Int, Integer))]
  , locals  :: [([Name], E)]
  , rules   :: [([Name], E, [([Name], E)], [Action'])]
  , methods :: [([Name], Method)]
  }

data ActionDB = ActionDB
  { guard   :: E
  , locals' :: [([Name], E)]
  , actions :: [Action']
  }

data Method
  = MethodValue       [Int] Int                           E
  | MethodAction      [Int]     E [([Name], E)] [Action']
  | MethodActionValue [Int] Int E [([Name], E)] [Action'] E

data Action'
  = Assign E E
  | Display String [E]
  | Finish
  deriving Show

-- | Expressions.
data E
  = EConst  Int Integer
  | EReg    [Name] Int  -- width
  | ELocal  [Name] Int  -- id, width
  | ESelect Int Int E
  | EConcat E E
  | EAdd    E E
  | ESub    E E
  | EMul    E E
  | ENot    E
  | EAnd    E E
  | EOr     E E
  | EXor    E E
  | EEq     E E
  | EMux    E E E
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

instance Monoid E where
  mempty  = 0%0
  mappend = EConcat

-- | Operations valid in both Design and Action monads.
class DesignAction m where
  -- | Create a local expression.
  local :: Name -> E -> m E

instance DesignAction Design where
  local name a = do
    d <- get
    put d { locals = locals d ++ [(path d ++ [name], a)] }
    return $ ELocal (path d ++ [name]) (width a)

instance DesignAction Action where
  local name a = do
    d <- lift get
    modify $ \ e -> e { locals' = locals' e ++ [(path d ++ [name], a)] }
    return $ ELocal (path d ++ [name]) (width a)

-- | Register declaration.
reg :: Name -> Int -> Integer -> Design E
reg name width value = do
  d <- get
  put d { regs = regs d ++ [(path d ++ [name], (width, value))] }
  return $ EReg (path d ++ [name]) width

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

-- | Defines a state transition rule.
(==>) :: Name -> Action () -> Design ()
name ==> rule = do
  modify $ \ d -> d { path = path d ++ [name] }
  ActionDB guard locals actions <- execStateT rule ActionDB { guard = true, locals' = [], actions = [] }
  modify $ \ d -> d { path = init $ path d, rules = rules d ++ [(path d ++ [name], guard, locals, actions)] }

-- | A non-state modifying method returning a value.
methodValue :: Name -> [Int] -> Int -> ([E] -> E) -> Design ()
methodValue name widths width f = modify $ \ d -> d { methods = methods d ++ [(path d ++ [name], MethodValue widths width result)] }
  where
  result = f [ ELocal ["arg" ++ show i] w | (w, i) <- zip widths [0 :: Int ..] ]

-- | A state modifying method.
methodAction :: Name -> [Int] -> ([E] -> Action ()) -> Design ()
methodAction name widths f = do
  modify $ \ d -> d { path = path d ++ [name] }
  ((), ActionDB guard locals actions) <- runStateT (f [ ELocal ["arg" ++ show i] w | (w, i) <- zip widths [0 :: Int ..] ]) ActionDB { guard = true, locals' = [], actions = [] }
  modify $ \ d -> d { path = init $ path d, methods = methods d ++ [(path d, MethodAction widths guard locals actions)] }

-- | A state modifying method that returns a value.
methodActionValue :: Name -> [Int] -> Int -> ([E] -> Action E) -> Design ()
methodActionValue name widths width f = do
  modify $ \ d -> d { path = path d ++ [name] }
  (result, ActionDB guard locals actions) <- runStateT (f [ ELocal ["arg" ++ show i] w | (w, i) <- zip widths [0 :: Int ..] ]) ActionDB { guard = true, locals' = [], actions = [] }
  modify $ \ d -> d { path = init $ path d, methods = methods d ++ [(path d, MethodActionValue widths width guard locals actions result)] }

-- | State update.
(<==) :: E -> E -> Action ()
a <== b = modify $ \ d -> d { actions = actions d ++ [Assign a b] }

-- | Guard conditions.
cond :: E -> Action ()
cond a = modify $ \ d -> d { guard = EAnd (guard d) a }

-- | Display something for simulation.
display :: String -> [E] -> Action ()
display str args = modify $ \ d -> d { actions = actions d ++ [Display str args] }

-- | Stop a simulation.
finish :: Action ()
finish = modify $ \ d -> d { actions = actions d ++ [Finish] }

-- | Constant bit vector given width and value.
(%) :: Int -> Integer -> E
(%) = EConst

-- | False constant.
false :: E
false = 1%0

-- | True constant.
true :: E
true = 1%1

-- | Bit range selection.
(#) :: E -> (Int, Int) -> E
a # (msb, lsb) = ESelect msb lsb a

-- | Single bit selection.
(##) :: E -> Int -> E
a ## b = a#(b,b)

-- | Bit vector equality.
(==.) :: E -> E -> E
(==.) = EEq

-- | Bit vector inequality.
(/=.) :: E -> E -> E
a /=. b = complement $ a ==. b

-- | Mux:  mux test onTrue onFalse
mux :: E -> E -> E -> E
mux = EMux


-- | Width of a bit vector.
width :: E -> Int
width a = case a of
  EConst w _ -> w
  EReg   _ w -> w
  ELocal _ w -> w
  EConcat a b -> width a + width b
  ESelect msb lsb _ -> msb - lsb + 1
  EAdd   a _ -> width a
  ESub   a _ -> width a
  EMul   a _ -> width a
  ENot   a   -> width a
  EAnd   a _ -> width a
  EOr    a _ -> width a
  EXor   a _ -> width a
  EEq    _ _ -> 1
  EMux   _ a _ -> width a

-- | Extract the bytes of a bit vector.
bytes :: E -> [E]
bytes a = [ a # (lsb + 8, lsb) | lsb <- reverse [0, 8 .. width a - 1] ]  -- Ignores msbs if not divisible by 8.

-- | Extract the bits of a bit vector.
bits :: E -> [E]
bits a = [ a # (i, i) | i <- reverse [0 .. width a - 1] ]



-- | Compiles a design.
compile :: FilePath -> Design () -> IO ()
compile file design = execStateT design DesignDB { path = [], regs = [], locals = [], rules = [], methods = [] } >>= writeFile file . bsv file

var :: [Name] -> String
var path = intercalate "$" path

bsv :: FilePath -> DesignDB -> String
bsv file (DesignDB _ regs locals rules methods) = unlines $
  [ printf "// Generated by AtomHD."
  , printf "package %s;" m
  , printf ""
  , printf "interface %s;" m
  , unlines $ map methodDecl methods
  , printf "endinterface"
  , printf ""
  , printf "module mk%s (%s);" m m
  , unlines [ printf "Reg#(Bit#(%d)) %s <- mkReg(%d);" w (var path) v | (path, (w, v)) <- regs ]
  , unlines [ printf "Bit#(%d) %s = %s;" (width a) (var path) (expr a) | (path, a) <- locals ]
  , unlines $ map rule rules
  , unlines $ map method methods
  , printf "endmodule"
  , printf "endpackage"
  ]
  where
  m = takeWhile (/= '.') file

expr :: E -> String
expr a = case a of
  EConst w v -> printf "%d'h%x" w v
  EReg   path _ -> var path
  ELocal path _ -> var path
  EConcat a b -> printf "{%s, %s}" (expr a) (expr b)
  ESelect msb lsb a -> printf "(%s)[%d:%d]" (expr a) msb lsb
  EAdd   a b -> printf "(%s + %s)"  (expr a) (expr b)
  ESub   a b -> printf "(%s - %s)"  (expr a) (expr b)
  EMul   a b -> printf "(%s * %s)"  (expr a) (expr b)
  ENot   a   -> printf "(~ %s)"     (expr a)
  EAnd   a b -> printf "(%s & %s)"  (expr a) (expr b)
  EOr    a b -> printf "(%s | %s)"  (expr a) (expr b)
  EXor   a b -> printf "(%s ^ %s)"  (expr a) (expr b)
  EEq    a b -> printf "pack(%s == %s)" (expr a) (expr b)
  EMux   a b c -> printf "(unpack(%s) ? %s : %s)" (expr a) (expr b) (expr c)

rule :: ([Name], E, [([Name], E)], [Action']) -> String
rule (path, guard, locals, actions) = unlines
  [ printf "rule %s (1'b1 == %s);" (var path) (expr guard)
  , unlines [ printf "  Bit#(%d) %s = %s;" (width a) (var path) (expr a) | (path, a) <- locals ]
  , unlines $ map action actions
  , printf "endrule"
  ]

method :: ([Name], Method) -> String
method a@(_, m) = case m of
  MethodValue _ _ result -> unlines
    [ methodDecl a
    , printf "  return %s;" (expr result)
    , printf "endmethod"
    ]
  MethodAction _ guard locals actions -> unlines
    [ init (methodDecl a) ++ " if (unpack(" ++ expr guard ++ "));"
    , unlines [ printf "  Bit#(%d) %s = %s;" (width a) (var path) (expr a) | (path, a) <- locals ]
    , unlines $ map action actions
    , printf "endmethod"
    ]
  MethodActionValue _ _ guard locals actions result -> unlines
    [ init (methodDecl a) ++ " if (unpack(" ++ expr guard ++ "));"
    , unlines [ printf "  Bit#(%d) %s = %s;" (width a) (var path) (expr a) | (path, a) <- locals ]
    , unlines $ map action actions
    , printf "  return %s;" (expr result)
    , printf "endmethod"
    ]

methodDecl :: ([Name], Method) -> String
methodDecl (path, a) = case a of
  MethodValue       widths width       _ -> printf "method Bit#(%d) %s%s;"               width (var path) (declArgs widths)
  MethodAction      widths       _ _ _   -> printf "method Action %s%s;"                       (var path) (declArgs widths)
  MethodActionValue widths width _ _ _ _ -> printf "method ActionValue#(Bit#(%d)) %s%s;" width (var path) (declArgs widths)
  where
  declArgs widths
    | null widths = ""
    | otherwise   = "(" ++ intercalate ", " [ printf "Bit#(%d) arg%d" w i | (w, i) <- zip widths [0 :: Int ..] ] ++ ")"

action :: Action' -> String
action a = case a of
  Assign a b       -> printf "  %s <= %s;" (expr a) (expr b)
  Display str args -> printf "  $display(\"%s\"%s);" str $ concatMap ((", " ++) . expr) args
  Finish           -> printf "  $finish;"
 
