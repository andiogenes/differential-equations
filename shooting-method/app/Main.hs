module Main where

import RungeKutta (Pair (..))
import ShootingMethod (solve, x0, xn)

solution h0 eps eps' = 
    solve x0 xn h0 eps eps' (Pair 10 10) 

main :: IO ()
main = putStr $ show $ solution 0.1 0.1 0.1
