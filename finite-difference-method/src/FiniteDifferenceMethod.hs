module FiniteDifferenceMethod 
  ( grid
  , solve1
  , solve2
  ) where

import Parameters

-- | Строит равномерную сетку ['from','to'] из 'n' частей.
grid :: (Ord a, Enum a, Fractional a) => a -> a -> a -> [a]
grid from to n = takeWhile (\x -> x <= to) [x | i <- [0 ..], let x = from + h * i]
  where
    h = (to - from) / n

-- | Находит решение первого порядка точности на сетке из 'n' частей.
solve1 :: Double -> [Double]
solve1 n = thomas as bs cs ds
  where
    h = (b - a) / n
    gs = take (length xs - 1) xs 
      where _:xs = grid a b n

    b0 = -f1*h + d1
    c0 = d1
    d0 = e1*h

    ai = [1 - (a' x)*h/2 | x <- gs]
    bi = [2 - (b' x)*h^2 | x <- gs]
    ci = [1 + (a' x)*h/2 | x <- gs]
    di = [(c x)*h^2 | x <- gs]

    an = -d2
    bn = -f2*h - d2
    dn = e2*h

    as = ai ++ [an]
    bs = b0:bi ++ [bn]
    cs = c0:ci
    ds = d0:di ++ [dn]

-- | Находит решение второго порядка точности на сетке из 'n' частей.
solve2 :: Double -> [Double]
solve2 n = thomas as bs cs ds
  where
    h = (b - a) / n
    gs = take (length xs - 1) xs 
      where _:xs = grid a b n

    b0 = -f1*h + d1 + d1*(a' a - (b' a)*h)*h/2
    c0 = (a' a) * d1 * h/2 + d1
    d0 = e1*h + (c a) * d1 * h^2/2

    ai = [1 - (a' x)*h/2 | x <- gs]
    bi = [2 - (b' x)*h^2 | x <- gs]
    ci = [1 + (a' x)*h/2 | x <- gs]
    di = [(c x)*h^2 | x <- gs]

    an = (a' b)*d2*h/2 - d2
    bn = -f2*h - d2 + d2*(a' b + (b' b)*h)*h/2
    dn = e2*h - (c b)*d2*(h^2/2)

    as = ai ++ [an]
    bs = b0:bi ++ [bn]
    cs = c0:ci
    ds = d0:di ++ [dn]

-- | Находит решение системы методом прогонки
thomas :: Fractional g => [g] -> [g] -> [g] -> [g] -> [g]
thomas as bs cs ds = rectify xs
  where
    n = length bs
    bs' = b (0) : [b (i) - (a (i) / b' (i - 1)) * (c (i - 1)) | i <- [1 .. n - 1]]
    ds' = d (0) : [d (i) - (a (i) / b' (i - 1)) * (d' (i - 1)) | i <- [1 .. n - 1]]
    xs = reverse $ d' (n - 1) / b' (n - 1) : [(d' (i) - c (i) * x (i + 1)) / b' (i) | i <- [n - 2, n - 3 .. 0]]

    a i = as !! (i - 1)
    b i = bs !! i
    c i = cs !! i
    d i = ds !! i
    x i = xs !! i
    b' i = bs' !! i
    d' i = ds' !! i

    rectify s = map abs s