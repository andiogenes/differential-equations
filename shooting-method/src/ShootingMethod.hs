module ShootingMethod
    ( rho
    , theta
    , tanAlpha
    , beta
    , x0
    , xn
    , f
    , cond
    , phi
    , solve
    ) where

import RungeKutta (Pair (..), rk4r, first)

rho :: Double
rho = 0.5

theta :: Double
theta = 0.05

tanAlpha :: Double
tanAlpha = tan alpha where alpha = 6 * (pi / 180)

beta :: Double
beta = 0.4

x0 :: Double
x0 = 0

xn :: Double
xn = 1

f :: Pair (Double -> Pair Double -> Double)
f = Pair u' y'
  where
    u' _ (Pair _ y) = y
    y' r (Pair u y) = (b r) * u ^ 4 - (a r) * y
      where
        a r = 1 / (r + rho) - tanAlpha / ((1 - r) * tanAlpha + theta)
        b r = beta / ((1 - r) * tanAlpha + theta)

cond :: Num a => a -> Pair a
cond s = Pair 1 s

phi :: Double -> Double -> [(Double, Pair Double)]
phi s h0 = rk4r f (cond s) x0 xn h0

solve :: Double -> Double -> Double -> Double -> Pair Double -> (Double, Pair Double)
solve s e h0 eps (Pair prev solution) =
    if (abs prev) < eps then (e, Pair prev solution)
    else
        let c = (s + e) / 2
            (_, ps) = head $ phi s h0
            (_, pe) = head $ phi e h0
            (_, pc) = head $ phi c h0
            (sNext, eNext, prevNext) = 
              if signum (first ps) /= signum (first pc) 
                then (s, c, pc) 
                else (c, e, pe)
        in solve sNext eNext h0 eps prevNext