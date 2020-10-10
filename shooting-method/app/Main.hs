module Main where

import RungeKutta (Pair (..))
import ShootingMethod (solve, x0, xn)

solution from to h0 eps = 
    solve from to h0 eps (Pair 10 10) 

main :: IO ()
main = putStr $ show $ solution (-3) (-0.5) 0.1 0.1
