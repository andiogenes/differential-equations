module Parameters
  (a', b', c, a, b, f1, d1, e1, f2, d2, e2, u)
  where

a' :: Fractional a => a -> a
a' x = (4 * x) / (2 * x + 1)

b' :: Fractional a => a -> a
b' x = -4 / (2 * x + 1)

c :: Num p1 => p2 -> p1
c _ = 0

a :: Double
b :: Double
f1 :: Double
d1 :: Double
e1 :: Double
f2 :: Double
d2 :: Double
e2 :: Double
(a, b, f1, d1, e1, f2, d2, e2) = (0, 1, 1, 1, 0, 2, 1, 3)

u :: Floating a => a -> a
u x = x + exp ((-2) * x)