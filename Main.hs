{-# Language TypeFamilies, DataKinds, KindSignatures, TypeOperators, UndecidableInstances, ScopedTypeVariables #-}
module Main where

import Data.Type.Natural
import Data.Proxy

data FB = Number Nat | Fizz | Buzz | FizzBuzz

type family Mod (m :: Nat) (n :: Nat) :: Nat
type instance Mod (S m) n = MN (S m) n (n :<<= m)
type family MN (m :: Nat) (n :: Nat) (ltePredM :: Bool) :: Nat
type instance MN (S m) n False = MN (S m) (n :- (S m)) (n :- (S m) :<<= m)
type instance MN m n True = n

type family ((n :: Nat) := (m :: Nat)) :: Bool
type instance (Z := Z) = True
type instance (Z := S n) = False
type instance (S n := Z) = False
type instance (S n := S m) = n := m

type family XFizzBuzz (n :: Nat) :: FB
type family XFB (n :: Nat) (mod3 :: Bool) (mod5 :: Bool) :: FB
type instance XFizzBuzz n = XFB n (Mod N3 n := N0) (Mod N5 n := N0)

type instance XFB n True True = FizzBuzz
type instance XFB n True False = Fizz
type instance XFB n False True = Buzz
type instance XFB n False False = Number n

-- for representation

class ToNat (n :: Nat) where toN :: Proxy n -> Int
instance ToNat Z where toN _ = 0
instance ToNat n => ToNat (S n)
  where toN _ = succ (toN (Proxy :: Proxy n))

class PrintFizzBuzz (fb :: FB) where showfb :: Proxy fb -> String

instance ToNat n => PrintFizzBuzz (Number n) where showfb _ = show $ toN (Proxy :: Proxy n)
instance PrintFizzBuzz Fizz where showfb _ = "Fizz"
instance PrintFizzBuzz Buzz where showfb _ = "Buzz"
instance PrintFizzBuzz FizzBuzz where showfb _ = "FizzBuzz"

main :: IO ()
main = do
  print (showfb (Proxy :: Proxy (XFizzBuzz N1)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N2)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N3)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N4)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N5)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N6)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N7)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N8)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N9)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N10)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N11)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N12)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N13)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N14)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N15)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N16)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N17)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N18)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N19)))
  print (showfb (Proxy :: Proxy (XFizzBuzz N20)))
