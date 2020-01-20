{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Except
import Control.Monad.List
import Control.Monad.State
import Control.Monad.Writer
import Data.Function
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.Ratio
import GHC.Real
import Math.NumberTheory.Euclidean
import qualified Math.NumberTheory.Primes as P
import qualified Math.NumberTheory.Primes.Testing as PT
import Text.PrettyPrint.HughesPJ hiding ((<>))

-- | The state of the compiler.
-- currInstPrime, nextInstPrime are integers representing the primes
-- to use for the current and next instructions.
-- labels, vars are mappings from strings to primes.
-- primes is the infinite list of primes provided by the arithmoi package
-- gensymCount is the counter for gensyms
data CompState =
  CompState
    { currInstPrime, nextInstPrime :: Integer
    , labels, vars :: M.Map String Integer
    , primes :: [P.Prime Integer]
    , gensymCount :: Integer
    }
  deriving (Show)

newtype Comp a =
  Comp
    { runComp :: ExceptT String (State CompState) a
    }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState CompState
           , MonadError String
           )

-- runComp :: Comp a -> Either String a
run a = a & runComp & runExceptT & flip evalState initState

initState =
  let (c:n:p) = P.primes
   in (CompState
         { currInstPrime = P.unPrime c
         , nextInstPrime = P.unPrime n
         , primes = p
         , labels = mempty
         , vars = mempty
         , gensymCount = 0
         })

newsym :: Comp String
newsym = do
  n <- gets gensymCount
  modify (\s -> s {gensymCount = n + 1})
  return ('t' : show n)

-- | Return a new prime we haven't used yet.
newPrime :: Comp Integer
newPrime = do
  l <- gets primes
  modify (\s -> s {primes = tail l})
  return (P.unPrime (head l))

-- | Advance the instruction primes.
advance :: Comp ()
advance = do
  c <- gets nextInstPrime
  p <- newPrime
  modify (\s -> s {currInstPrime = c, nextInstPrime = p})

-- | Return the prime for a given label.
primeForLabel :: String -> Comp Integer
primeForLabel label = do
  labels <- gets labels
  case M.lookup label labels of
    Just p -> return p
    Nothing -> do
      p <- newPrime
      modify (\s -> s {labels = M.insert label p labels})
      return p

-- | Return the prime for a given variable.
primeForVar :: String -> Comp Integer
primeForVar label = do
  labels <- gets vars
  case M.lookup label labels of
    Just p -> return p
    Nothing -> do
      p <- newPrime
      modify (\s -> s {vars = M.insert label p labels})
      return p

-- | FracComp is a DSL in the tagless final style for Turing-complete
-- FRACTRAN programs.
class Monad repr =>
      FracComp repr
  -- Minimal instructions needed for Turing-completeness
  where
  {-# MINIMAL lit, label, addi, jge, gensym #-}
  lit :: Integer -> repr [Rational]
  label :: String -> repr [Rational]
  addi :: String -> Integer -> repr [Rational]
  jge :: String -> Integer -> String -> repr [Rational]
  gensym :: repr String
  subi :: String -> Integer -> repr [Rational]
  subi x y = addi x (-y)
  adds :: String -> String -> repr [Rational]
  -- | Add with store
  adds a b = do
    gtemp <- gensym
    assemble
      [ while (jge b 1) [addi gtemp 1, subi b 1]
      , while (jge gtemp 1) [addi a 1, addi b 1, subi gtemp 1]
      ]
  muls :: String -> String -> repr [Rational]
  -- | Multiply with store
  muls a b = do
    gtemp <- gensym
    grestore <- gensym
    assemble
      [ move grestore b
      , while (jge b 1) [adds gtemp a, subi b 1]
      , move a gtemp
      , move b grestore
      , zero gtemp
      , zero grestore
      ]
  -- | Goto statement.
  goto :: String -> repr [Rational]
  goto dest = do
    g <- gensym
    jge g 0 dest
  -- | While loop, taking a test and a body.
  while :: (String -> repr [Rational]) -> [repr [Rational]] -> repr [Rational]
  while test body = do
    gstart <- gensym
    gend <- gensym
    assemble
      (concat [[goto gend, label gstart], body, [label gend, test gstart]])
  -- | Jump if less than statement.
  jle :: String -> Integer -> String -> repr [Rational]
  jle var val dest = do
    skip <- gensym
    assemble [jge var (val + 1) skip, goto dest, label skip]

-- | Zero a given variable.
zero var = while (jge var 1) [subi var 1]

-- | Move statement.
move to from = do
  gtemp <- gensym
  assemble
    [ zero to
    , while (jge from 1) [addi gtemp 1, subi from 1]
    , while (jge gtemp 1) [addi to 1, addi from 1, subi gtemp 1]
    ]

-- | Immediate modulo of a variable with an integer.
modi var val = assemble [while (jge var val) [subi var val]]

-- | Immediate division of a variable with an integer.
divi var val = do
  gresult <- gensym
  assemble
    [ zero gresult
    , while (jge var val) [subi var val, addi gresult 1]
    , move var gresult
    ]

-- Used in the pretty printer
newtype S a =
  S
    { unS :: StateT Int (Writer [Doc]) a
    }
  deriving (Functor, Applicative, Monad, MonadWriter [Doc], MonadState Int)

instance FracComp S where
  lit i = tell [text (show i)] $> []
  label l = tell ["label" <+> text l] $> []
  addi l x = tell [text l <+> "+=" <+> text (show x)] $> []
  jge l x dest = tell [text l <+> ">=" <+> (text (show x) <+> text dest)] $> []
  gensym = gets (('g' :) . show) <* modify (+ 1)
  jle l x dest = tell [text l <+> "<=" <+> (text (show x) <+> text dest)] $> []
  adds l x = tell [text l <+> "+=" <+> text x] $> []
  subi l x = tell [text l <+> "-=" <+> text (show x)] $> []
  goto l = tell ["goto" <+> text l] $> []
  while test body = do
    censor (fmap (\x -> "while " <> x <> "{")) (test "")
    censor (nest 2 <$>) (sequence body)
    tell ["}"]
    return []

view = unS

pretty x = vcat (execWriter (evalStateT (sequence (view <$> x)) 0))

instance FracComp Comp where
  lit i = return [toRational i]
  label s = do
    f <- (%) <$> gets currInstPrime <*> primeForLabel s
    return [f]
  -- need less than 0 check because (^) raises an error on
  -- negative exponent
  addi x 0 = primeForVar x $> []
  addi x y = do
    g <- (^ abs y) <$> primeForVar x
    f <-
      if y < 0
        then (%) <$> gets nextInstPrime <*> ((* g) <$> gets currInstPrime)
        else (%) <$> ((* g) <$> gets nextInstPrime) <*> gets currInstPrime
    advance
    return [f]
  jge var val label = do
    restore <- newPrime
    a <-
      (restore %) <$>
      ((*) <$> ((^ val) <$> primeForVar var) <*> gets currInstPrime)
    b <-
      (% restore) <$>
      ((*) <$> primeForLabel label <*> ((^ val) <$> primeForVar var))
    c <- (%) <$> gets nextInstPrime <*> gets currInstPrime
    advance
    return [a, b, c]
  gensym = newsym

-- | The assembler.
assemble :: (Traversable m, Monad m, Monad f) => m (f (m a)) -> f (m a)
assemble l = join <$> sequence l

-- | Naïve interpreter for FRACTRAN (credit to Stuart Geipel).  We can
-- do much better, see https://github.com/pimlu/fractran for details.
naive fs n = maybe [] (next . numerator) match
  where
    ps = map (* fromIntegral n) fs
    match = find ((== 1) . denominator) ps
    next p = p : naive fs p

runAssembler = run . assemble

runAsmWith f l =
  case runAssembler l of
    Right l -> f (naive l 2)
    Left err -> putStrLn ("Failed: " ++ err)

-- | Run a FRACTRAN program, logging each execution.
runAsmVerbose :: [Comp [Rational]] -> IO ()
runAsmVerbose = runAsmWith (mapM_ print . (P.factorise <$>))

-- | Run a FRACTRAN program, printing the final result, only if the
-- machine terminates.
runAsm :: [Comp [Rational]] -> IO ()
runAsm = runAsmWith (print . last . (P.factorise <$>))

-- Space efficient (i.e. register-minimal) Fibonacci. The result of
-- the computation is stored in registers a and b, and depends on the
-- parity of n.
fibProg n =
  [ addi "a" 1
  , addi "b" 1
  , addi "n" n
  , while
      (jge "n" 1)
      [ adds "a" "b"
      , subi "n" 1
      , jle "n" 0 "end"
      , adds "b" "a"
      , subi "n" 1
      , jle "n" 0 "end"
      ]
  , label "end"
  ]

sumTo n = [addi "c" 0, addi "n" n, while (jge "n" 0) [adds "c" "n", subi "n" 1]]

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- | Perform peephole optimization until the program no longer changes
peepholeOptimize = until (\x -> x == peephole x) peephole

{- | Perform a single peephole pass, starting from the end of the list
and going backwards, this is so that optimizations can propagate
from the end of the list, for instance,
> peephole [3%2,5%3,7%5,11%7,13%11] == [13 % 2]
-}
peephole :: Foldable t => t (Ratio Integer) -> [Ratio Integer]
peephole = foldr g []
  where
    -- | In the program fragment: '[a/b, c/d] ++ e' if a and d are the
    -- same prime, and c and b are coprime, then we can write '[c/b]
    -- ++ e' instead.  However, we have to check if the prime that we
    -- eliminated (i.e. a) appears anywhere in e before doing so.
    -- This check is done by the function f.
    optimizable (a :% b) (c :% d) e =
      a == d && PT.isPrime a && coprime c b && f a e
    -- | Check if the prime 'a' appears anywhere in the program
    -- segment 'e', by collecting all the numerators and denominators
    -- and checking if 'a' is not in 'e'.  Note that since in
    -- 'optimizable' we know d is the same prime as a, it suffices to
    -- check using 'notElem' instead of some modulo check, as (c/d)'s
    -- execution would always shadow that of some other fraction (X/d)
    -- later.
    f a e = a `notElem` (e >>= h)
    h (a :% b) = [a, b]
    g x [] = [x]
    g x@(_ :% b) xs@(y@(c :% _):ys)
      | optimizable x y ys = c % b : ys
      | otherwise = x : xs

-- | Check if peephole optimization yielded a program that had the same
-- final end state.
peepholeCorrect :: [Ratio Integer] -> Bool
peepholeCorrect p = ((==) `on` (last . flip naive 2)) p (peepholeOptimize p)
