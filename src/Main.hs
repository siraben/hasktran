{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad.Except
import Control.Monad.List
import Control.Monad.State
import Control.Monad.Writer
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Ratio
import qualified Math.NumberTheory.Primes as P

-- |The state of the compiler.
-- currInstPrime, nextInstPrime are integers representing the primes
-- to use for the current and next instructions.
-- labels, vars are mappings from strings to primes. 
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

initState = let (c:n:p) = P.primes in
  (CompState
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

-- |Return a new prime we haven't used yet.
newPrime :: Comp Integer
newPrime = do
  l <- gets primes
  modify (\s -> s {primes = tail l})
  return (P.unPrime (head l))

-- |Advance the instruction primes.
advance :: Comp ()
advance = do
  c <- gets nextInstPrime
  p <- newPrime
  modify (\s -> s {currInstPrime = c, nextInstPrime = p})

-- |Return the prime for a given label.
primeForLabel :: String -> Comp Integer
primeForLabel label = do
  labels <- gets labels
  case M.lookup label labels of
    Just p -> return p
    Nothing -> do
      p <- newPrime
      modify (\s -> s {labels = M.insert label p labels})
      return p

-- |Return the prime for a given variable.
primeForVar :: String -> Comp Integer
primeForVar label = do
  labels <- gets vars
  case M.lookup label labels of
    Just p -> return p
    Nothing -> do
      p <- newPrime
      modify (\s -> s {vars = M.insert label p labels})
      return p

-- |FracComp is a DSL in the tagless final style for Turing-complete
-- FRACTRAN programs.
class Monad repr =>
      FracComp repr
  where
  lit :: Integer -> repr [Rational]
  label :: String -> repr [Rational]
  addi :: String -> Integer -> repr [Rational]
  jge :: String -> Integer -> String -> repr [Rational]
  gensym :: repr String

-- Used in the pretty printer
newtype S a =
  S
    { unS :: StateT Int (Writer [String]) a
    }
  deriving (Functor, Applicative, Monad, MonadWriter [String], MonadState Int)

instance FracComp S where
  lit i = tell [show i] *> return []
  label l = tell [unwords ["label", l]] *> return []
  addi l x = tell [unwords ["addi", l, show x]] *> return []
  jge l x dest = tell [unwords ["jge", l, show x, dest]] *> return []
  gensym = do
    x <- get
    modify (+ 1)
    return ('g' : show x)

view = unS

pretty x = unlines (execWriter (evalStateT (sequence (view <$> x)) 0))

instance FracComp Comp where
  lit i = return [toRational i]
  label s = do
    f <- (%) <$> gets currInstPrime <*> primeForLabel s
    return [f]
  -- need less than 0 check because (^) raises an error on
  -- negative exponent
  addi x y = do
    g <- (^) <$> primeForVar x <*> return (abs y)
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

subi x y = addi x (-y)

-- |Add with store
adds a b = do
  gtemp <- gensym
  assemble
    [ while (jge b 1) [addi gtemp 1, subi b 1]
    , while (jge gtemp 1) [addi a 1, addi b 1, subi gtemp 1]
    ]

-- |Goto statement.
goto dest = do
  g <- gensym
  jge g 0 dest

-- |Jump if less than statement.
jle var val dest = do
  skip <- gensym
  assemble [jge var (val + 1) skip, goto dest, label skip]

-- |While loop, taking a test and a body.
while test body = do
  gstart <- gensym
  gend <- gensym
  assemble (concat [[goto gend, label gstart], body, [label gend, test gstart]])

-- |Zero a given variable.
zero var = while (jge var 1) [subi var 1]

-- |Move statement.
move to from = do
  gtemp <- gensym
  assemble
    [ zero to
    , while (jge from 1) [addi gtemp 1, subi gtemp 1]
    , while (jge gtemp 1) [addi to 1, addi from 1, subi gtemp 1]
    ]

-- |Immediate modulo of a variable with an integer.
modi var val = assemble [while (jge var val) [subi var val]]

divi var val = do
  gresult <- gensym
  assemble
    [ zero gresult
    , while (jge var val) [subi var val, addi gresult 1]
    , move var gresult
    ]

-- |The assembler.
assemble :: (Traversable m, Monad m, Monad f) => m (f (m a)) -> f (m a)
assemble l = join <$> sequence l

-- |Naïve interpreter for FRACTRAN (credit to Stuart Geipel).  We can
-- do much better, see https://github.com/pimlu/fractran for details.
naive fs n = maybe [] (next . numerator) match
  where
    ps = map (* fromIntegral n) fs
    match = find ((== 1) . denominator) ps
    next p = p : naive fs p

runAssembler = run . assemble

runAsmWith f l =
  case runAssembler l of
    Right l -> f $ (P.factorise <$> naive l 2)
    Left err -> putStrLn ("Failed: " ++ err)

-- |Run a FRACTRAN program, logging each execution.
runAsmVerbose :: [Comp [Rational]] -> IO ()
runAsmVerbose = runAsmWith (mapM_ print)

-- |Run a FRACTRAN program, printing the final result, only if the
-- machine terminates.
runAsm :: [Comp [Rational]] -> IO ()
runAsm = runAsmWith (print . last)

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
