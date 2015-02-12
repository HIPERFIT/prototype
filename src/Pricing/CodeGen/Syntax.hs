{-# OPTIONS_GHC 
    -XTypeSynonymInstances 
    -XPatternSignatures 
    -XFlexibleContexts 
    -XFlexibleInstances
    -w  #-}

-- taken from LexiFi project repository
module CodeGen.Syntax where

import Data.Ord(comparing)

data Raw = ADD Raw Raw | MUL Raw Raw | NEGATE Raw | DIV Raw Raw
         | REAL Double -- literal
         | INT Int     -- second literal type
         | VAR String  -- variable (untyped)
         | SQRT Raw -- exemplified and for special needs...
           deriving(Eq,Read)

newtype Exp t = E ([String] -> Raw)

type RealE    = Exp Double
type IntE     = Exp Int

-- expression code generation:

instance Show Raw where 
 show (ADD r1 r2) = show r1 ++ "+" ++ show r2
  -- setting brackets when necessary
 show (MUL r1@(ADD _ _) r2@(ADD _ _))
                   = "(" ++ show r1 ++ ")*(" ++ show r2 ++ ")"
 show (MUL r1@(ADD _ _) r2) 
                   = "(" ++ show r1 ++ ")*" ++ show r2
 show (MUL r1 r2@(ADD _ _))
                   = show r1 ++ "*(" ++ show r2 ++ ")"
 show (MUL r1 r2) = show r1 ++ "*" ++ show r2
  -- same here, setting brackets
 show (DIV r1@(ADD _ _) r2@(ADD _ _))
                   = "(" ++ show r1 ++ ")/(" ++ show r2 ++ ")"
 show (DIV r1@(ADD _ _) r2) 
                   = "(" ++ show r1 ++ ")/" ++ show r2
 show (DIV r1 r2@(ADD _ _))
                   = show r1 ++ "/(" ++ show r2 ++ ")"
 show (DIV r1 r2) = "(" ++ show r1 ++ "/" ++ show r2 ++ ")"
 show (NEGATE r)  = "(-" ++ show r ++ ")"
 show (REAL i) = show i
 show (SQRT r) = "sqrt( " ++ show r ++ ")"
 show (VAR s)     = '(':s ++ ")"
 show (INT i)  = show i

instance Show (Exp t) where
  showsPrec i (E a) = showsPrec i r where
    r = a [c:i | i <- ("":map show [1..]), c<- ['a'..'z']]

var :: String -> Exp t
var s = E (\_ -> VAR s) -- XXX should check string and e.g. disallow operators

--------IntE -----------------
instance Eq IntE where
  (==) (E e1) (E e2) = e1 infiniteVars == e2 infiniteVars

instance Num IntE where 
  (+)         = typ2 ADD
  (*)         = typ2 MUL
  negate      = typ1 NEGATE
  fromInteger a = E(\ns -> INT (fromInteger a))

instance Integral IntE where
  toInteger (E e1) = truncate (eval (e1 infiniteVars)) -- may fail!

cmpRaw :: (Integer -> Integer -> Bool) -> Raw -> Raw -> Integer
cmpRaw rel r1 r2 = if rel (evalInt r1) (evalInt r2) then 1 else 0

-- -- missing: Enum and Real instance of IntE
instance Enum IntE where
   toEnum     n   = E (\_ -> INT n)
   fromEnum (E e) = fromEnum (eval (e infiniteVars))

instance Real IntE where
   toRational (E e) = toRational (eval (e infiniteVars))

-- and for Real, we need Ord... 
instance Ord IntE where
   compare = comparing toRational

--------------------------------------------
infiniteVars = [c:i | i <- ("":map show [1..]), c<- ['a'..'z']]

raw :: Exp t -> Raw
raw expt = make expt infiniteVars

---------------------------------------------
make :: Exp t -> [String] -> Raw
make (E a) ns = a ns

typ1 :: (Raw -> Raw) -> (Exp a -> Exp b)
typ1 f (E e1) = E (\ns -> f (e1 ns))

typ2 :: (Raw -> Raw -> Raw) -> (Exp a -> Exp b -> Exp c)
typ2 f (E e1) (E e2) = E (\ns -> f (e1 ns) (e2 ns))

typ3 :: (Raw -> Raw -> Raw -> Raw) -> (Exp a -> Exp b -> Exp c -> Exp d)
typ3 f (E e1) (E e2) (E e3) = E (\ns -> f (e1 ns) (e2 ns) (e3 ns))

-----------------------------------------------
realE :: (Show a, Num a) => a -> Exp Double
realE x = E (\ns -> REAL (read (show x))) -- XXXX dirty hack!

intE :: (Show a, Integral a) => a -> Exp Int
intE x = E (\ns -> INT (fromIntegral x))

--------RealE -----------------
instance Eq RealE where -- syntactic equality!
  (==) (E e1) (E e2) = e1 infiniteVars == e2 infiniteVars

instance Num RealE where 
  (+)         = typ2 ADD
  -- (-) not necessary if negate defined
  (*)         = typ2 MUL
  negate      = typ1 NEGATE
  fromInteger a = E(\ns -> REAL (fromInteger a))

instance Fractional RealE where
  (/) = typ2 DIV
  recip = typ1 (error "no recip") -- should we have it? Then without div
  fromRational r = E ( \ns -> REAL (fromRational r))

-- use special constructors and show instances for these (as hacked in here)
instance Floating RealE where
  pi = var "pi"
  sqrt = typ1 SQRT
{-
  exp x :: a -> a
  log :: a -> a
  (**) :: a -> a -> a
  logBase :: a -> a -> a
  sin :: a -> a
  tan :: a -> a
  cos :: a -> a
  asin :: a -> a
  atan :: a -> a
  acos :: a -> a
  sinh :: a -> a
  tanh :: a -> a
  cosh :: a -> a
  asinh :: a -> a
  atanh :: a -> a
  acosh :: a -> a
-}
 
-- instance Real RealE where
--   toRational (E e1) = toRational $ eval (e1 infiniteVars) -- may fail! XXXX

-- for Real, we need Ord... 
-- instance Ord RealE where
--   compare = comparing eval -- doznwok! XXXX

------------------------
-- "doing stuff" with it

eval :: Raw -> Double
eval (INT i)     = fromIntegral i
eval (REAL n)    = n
eval (ADD r1 r2) = eval r1 + eval r2
eval (MUL r1 r2) = eval r1 * eval r2
eval (NEGATE r)  = - eval r
eval (VAR s) = error ("met variable " ++ s) 
eval (SQRT x) = sqrt (eval x)

evalInt = truncate . eval

simplify :: Raw -> Raw
simplify = error "use eval to evaluate literals as far as possible"

