module Peano
  where

data Nat = Z | S Nat deriving (Show, Eq)

instance Ord Nat where 
  compare Z Z = EQ 
  compare Z _ = LT 
  compare _ Z = GT 
  compare (S m) (S n) = compare m n 

instance Enum Nat where
  succ n = S n
  pred Z     = Z
  pred (S n) = n
  toEnum n | n < 0     = error "No negative Nats"
           | n == 0    = Z
           | otherwise = S (toEnum (n-1))
  
  fromEnum Z     = 0
  fromEnum (S n) = 1 + fromEnum n

instance Num Nat where
  n + m    = add n m
  n - m    = sub n m
  n * m    = mul n m
  negate Z = Z
  negate _ = error "Cannot negate a Nat"
  signum Z = 0
  signum _ = 1
  abs n    = n
  fromInteger n = toEnum (fromIntegral n)

instance Real Nat where
  toRational n = fromIntegral (fromEnum n)
  
instance Integral Nat where
  toInteger n = fromIntegral (fromEnum n)
  quotRem n d  | d > n     = (Z,n)
               | otherwise = (S q, r) where 
                   (q,r) = quotRem (n-d) d

add :: Nat -> Nat -> Nat
add Z n = n
add (S n) m = add n (S m)

sub :: Nat -> Nat -> Nat
sub Z m = m
sub _ Z = Z
sub (S n) (S m) = sub n m

mul :: Nat -> Nat -> Nat
mul (S Z) m = m
mul n (S Z) = n
mul Z _     = Z
mul _ Z     = Z
mul (S n) m = mul n (add m m)

