{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Monad
import Control.Monad.Except
import Control.Monad.List
import Control.Monad.State
import Data.Function
import Data.List
import qualified Data.Map as M
import Data.Ratio
import qualified Math.NumberTheory.Primes as P

default (Integer, Rational)

data CompState =
  CompState
    { currInstPrime :: Integer
    , nextInstPrime :: Integer
    , primes :: [Integer]
    , labels :: M.Map String Integer
    , vars :: M.Map String Integer
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
run a = a & runComp & runExceptT & evalWithState initState
  where
    evalWithState s a = evalState a s

initState =
  (CompState
     { currInstPrime = 2
     , nextInstPrime = 3
     , primes = drop 2 (P.unPrime <$> P.primes)
     , labels = mempty
     , vars = mempty
     , gensymCount = 0
     })

newsym :: Comp String
newsym = do
  n <- gets gensymCount
  modify (\s -> s {gensymCount = n + 1})
  return ('t' : show n)

newPrime :: Comp Integer
newPrime = do
  l <- gets primes
  modify (\s -> s {primes = tail l})
  return (head l)

advance :: Comp ()
advance = do
  c <- gets nextInstPrime
  p <- newPrime
  modify (\s -> s {currInstPrime = c, nextInstPrime = p})

test = replicateM 4 (gets currInstPrime <* advance)

primeForLabel :: String -> Comp Integer
primeForLabel label = do
  labels <- gets labels
  case M.lookup label labels of
    Just p -> return p
    Nothing -> do
      p <- newPrime
      modify (\s -> s {labels = M.insert label p labels})
      return p

primeForVar :: String -> Comp Integer
primeForVar label = do
  labels <- gets vars
  case M.lookup label labels of
    Just p -> return p
    Nothing -> do
      p <- newPrime
      modify (\s -> s {vars = M.insert label p labels})
      return p

-- Tagless Final DSL
class Monad repr =>
      FracComp repr
  where
  lit :: Integer -> repr [Rational]
  label :: String -> repr [Rational]
  addi :: String -> Integer -> repr [Rational]
  jge :: String -> Integer -> String -> repr [Rational]
  gensym :: repr String

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

-- Perform a <- a + b
adds a b = do
  gtemp <- gensym
  assemble
    [ while (jge b 1) [addi gtemp 1, subi b 1]
    , while (jge gtemp 1) [addi a 1, addi b 1, subi gtemp 1]
    ]

goto dest = do
  g <- gensym
  jge g 0 dest

jle var val dest = do
  skip <- gensym
  assemble [jge var (val + 1) skip, goto dest, label skip]

while test body = do
  gstart <- gensym
  gend <- gensym
  assemble (concat [[goto gend, label gstart], body, [label gend, test gstart]])

zero var = while (jge var 1) [subi var 1]

move to from = do
  gtemp <- gensym
  assemble
    [ zero to
    , while (jge from 1) [addi gtemp 1, subi gtemp 1]
    , while (jge gtemp 1) [addi to 1, addi from 1, subi gtemp 1]
    ]

modi var val = assemble [while (jge var val) [subi var val]]

divi var val = do
  gresult <- gensym
  assemble
    [ zero gresult
    , while (jge var val) [subi var val, addi gresult 1]
    , move var gresult
    ]

assemble :: (Traversable m, Monad m, Monad f) => m (f (m a)) -> f (m a)
assemble l = join <$> sequence l

naive fs n = maybe [] (next . numerator) match
  where
    ps = map (* fromIntegral n) fs
    match = find ((== 1) . denominator) ps
    next p = p : naive fs p

runAssembler = run . assemble

runAsmVerbose l =
  case runAssembler l of
    Right l -> mapM_ print (P.factorise <$> naive l 2)
    Left err -> putStrLn ("Failed: " ++ err)

runAsm l =
  case runAssembler l of
    Right l -> print . last $ (P.factorise <$> naive l 2)
    Left err -> putStrLn ("Failed: " ++ err)

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

sumTo n = [addi "n" n, while (jge "n" 0) [adds "c" "n", subi "n" 1]]

main :: IO ()
main = putStrLn "Hello, Haskell!"
