{-# Language TypeFamilies, DataKinds, KindSignatures, TypeOperators, UndecidableInstances, ScopedTypeVariables #-}
module Main where

import Data.Type.Natural
import Data.Proxy

data FB = Number Nat | Fizz | Buzz | FizzBuzz

type family Add15 (fb :: FB) :: FB
type instance Add15 (Number n) = Number (n :+ N15)
type instance Add15 Fizz = Fizz
type instance Add15 Buzz = Buzz
type instance Add15 FizzBuzz = FizzBuzz

type family XFizzBuzz (n :: Nat) :: FB
type instance XFizzBuzz Z = FizzBuzz
type instance XFizzBuzz N1 = Number N1
type instance XFizzBuzz N2 = Number N2
type instance XFizzBuzz N3 = Fizz
type instance XFizzBuzz N4 = Number N4
type instance XFizzBuzz N5 = Buzz
type instance XFizzBuzz N6 = Fizz
type instance XFizzBuzz N7 = Number N7
type instance XFizzBuzz N8 = Number N8
type instance XFizzBuzz N9 = Fizz
type instance XFizzBuzz N10 = Buzz
type instance XFizzBuzz N11 = Number N11
type instance XFizzBuzz N12 = Fizz
type instance XFizzBuzz N13 = Number N13
type instance XFizzBuzz N14 = Number N14
type instance XFizzBuzz N15 = FizzBuzz
type instance XFizzBuzz (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S n)))))))))))))))) = Add15 (XFizzBuzz (S n))

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
