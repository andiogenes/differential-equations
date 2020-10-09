module RungeKutta
    ( rk4
    , rkf24
    ) where

rk4 :: (Fractional a, Ord a, Enum a) => (a -> a -> a) -> a -> a -> a -> a -> [(a, a)]
rk4 f u0 x0 xn h =
    let grid = takeWhile (\x -> x <= xn) [ x | i <- [0..], let x = x0+h*i ]
    
        fill _ [] acc = reverse acc
        fill u (x:xs) acc = 
            let u' = rk f h x u
            in fill u' xs $ (x,u):acc

    in fill u0 grid []


rkf24 :: (Floating p, Ord p) => (p -> p -> p) -> p -> p -> p -> p -> p -> [(p, p)]
rkf24 f u0 x0 xn h0 eps =
    let fac = 0.8
        facMin = 0.4
        facMax = 4.0
        facMaxBad = 1.0

        fill x u h table stepFacMax = 
            if x >= xn then reverse table
            else 
                let h' = if x + h > xn then xn - x else h
                    (y, e) = egorov f h' x u
                    err = abs e

                    hNext = h' * (min stepFacMax $ max facMin $ fac * ((eps/err)**0.2))
                    (x', u', fm') = 
                        if err <= eps then (x + h', y, facMax)
                        else (x, u, facMaxBad)

                    t' = if err <= eps then (x', u'):table else table

                in fill x' u' hNext t' fm'

    in fill x0 u0 h0 [(x0, u0)] facMax


nomials :: Fractional t => (t -> t -> t) -> t -> t -> t -> (t, t, t, t)
nomials f h x u = (k1, k2, k3, k4)
    where k1 = h * f x u
          k2 = h * f (x + h/2) (u + k1/2)
          k3 = h * f (x + h/2) (u + k2/2)
          k4 = h * f (x + h) (u + k3)


rk :: Fractional a => (a -> a -> a) -> a -> a -> a -> a
rk f h x u =
    let (k1, k2, k3, k4) = nomials f h x u
    in u + (k1 + 2*k2 + 2*k3 + k4) / 6


egorov :: Fractional b => (b -> b -> b) -> b -> b -> b -> (b, b)
egorov f h x u =
    let (k1, k2, k3, k4) = nomials f h x u
        u' = u + (k1 + 2*k2 + 2*k3 + k4) / 6
        e = u' - (u + (-k1 + 2*k2 + 2*k3 - k4) / 2)
    in (u', e)