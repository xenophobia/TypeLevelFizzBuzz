{-# Language TypeFamilies, DataKinds, KindSignatures, TypeOperators, UndecidableInstances, ScopedTypeVariables #-}
module Main where

import Data.Type.Natural
import Data.Proxy

data FB = Number Nat | Fizz | Buzz | FizzBuzz

type family Mod3 (n :: Nat) :: Bool
type instance Mod3 n = M3 n (n :<<= N2)
type family M3 (n :: Nat) (lte3 :: Bool) :: Bool
type instance M3 n False = M3 (n :- N3) (n :- N3 :<<= N2)
type instance M3 N0 True = True
type instance M3 N1 True = False
type instance M3 N2 True = False

type family Mod5 (n :: Nat) :: Bool
type instance Mod5 n = M5 n (n :<<= N4)
type family M5 (n :: Nat) (lte5 :: Bool) :: Bool
type instance M5 n False = M5 (n :- N5) (n :- N5 :<<= N4)
type instance M5 N0 True = True
type instance M5 N1 True = False
type instance M5 N2 True = False
type instance M5 N3 True = False
type instance M5 N4 True = False

type family XFizzBuzz (n :: Nat) :: FB
type family XFB (n :: Nat) (mod3 :: Bool) (mod5 :: Bool) :: FB
type instance XFizzBuzz n = XFB n (Mod3 n) (Mod5 n)

type instance XFB n True True = FizzBuzz
type instance XFB n True False = Fizz
type instance XFB n False True = Buzz
type instance XFB n False False = Number n

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
