module Main where

import FiniteDifferenceMethod
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Easy
import Parameters

steps = 500

main :: IO ()
main = toFile def "solition.svg" $ do
  layout_title .= "Solution"
  setColors [opaque blue, opaque green, opaque red]
  plot (line "u (1st order)" [u1])
  plot (line "u (2nd order)" [u2])
  plot (line "u (exact)" [u3])
  where
    values = grid a b steps
    u1 = zip values (solve1 steps)
    u2 = zip values (solve2 steps)
    u3 = zip values [u x | x <- values]

norm :: IO ()
norm = toFile def "norm.svg" $ do
  layout_title .= "Norm"
  setColors [opaque blue, opaque green, opaque red, opaque orange]
  plot (line "u (1st order)" [zip values u1])
  plot (line "u (2nd order)" [zip values u2])

  plot (line "u (1nd order) norm" [map (\x -> (x, u1max)) values])
  plot (line "u (2nd order) norm" [map (\x -> (x, u2max)) values])
  where
    values = grid a b steps

    u1 = zipWith (\x y -> abs (x - y)) u3 $ solve1 steps
    u2 = zipWith (\x y -> abs (x - y)) u3 $ solve2 steps
    u3 = [u x | x <- values]

    u1max = maximum u1
    u2max = maximum u2
