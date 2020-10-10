module Main where

import Pair (Pair (..))
import ShootingMethod (solve, phi)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

solution = solve from to step eps (Pair 10 10)
    where
        from    = -5
        to      = 0
        step    = 0.001
        eps     = 0.0000001 

table = reverse $ phi s step
    where
        (s, _)  = solution
        step    = 0.001

main :: IO ()
main = toFile def "solution.svg" $ do
    layout_title .= "Solution"
    setColors [opaque blue, opaque green]
    plot (line "u" [u])
    plot (line "u'" [u'])
    where
        (u, u') = unzip $ map (\(x, Pair u u') -> ((x, u),(x, u'))) table