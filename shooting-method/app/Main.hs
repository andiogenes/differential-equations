module Main where

import Pair (Pair (..))
import ShootingMethod (solve, x0, xn)

solution from to h0 eps = 
    solve from to h0 eps (Pair 10 10) 

main :: IO ()
main = putStr $ show $ solution (-3) (-0.5) 0.001 0.0000001
