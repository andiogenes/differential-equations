module Pair
    ( Pair (..)
    , first
    , second
    ) where

data Pair a = Pair a a deriving (Eq, Show)

instance Num a => Num (Pair a) where
    Pair a b + Pair c d         = Pair (a+c) (b+d)
    Pair a b * Pair c d         = Pair (a*c) (b*d)
    Pair a b - Pair c d         = Pair (a-c) (b-d)
    abs (Pair a b)              = Pair (abs a) (abs b)
    signum (Pair a b)           = Pair (signum a) (signum b)
    fromInteger i               = Pair (fromInteger i) (fromInteger i)

instance Fractional a => Fractional (Pair a) where
    Pair a b / Pair c d         = Pair (a/c) (b/d)
    fromRational r              = Pair (fromRational r) (fromRational r)

instance Functor Pair where
    fmap f (Pair x y)           = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

first :: Pair a -> a
first (Pair l _) = l

second :: Pair a -> a
second (Pair _ r) = r