module RungeKutta
    ( rk4
    , rkf24
    , rk4r
    , rkf24r
    , Pair (..)
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


rk4 :: (Fractional a, Ord a, Enum a) => Pair (a -> Pair a -> a) -> Pair a -> a -> a -> a -> [(a, Pair a)]
rk4 f u0 x0 xn h = reverse $ rk4r f u0 x0 xn h

rkf24 :: (Floating p, Ord p) => Pair (p -> Pair p -> p) -> Pair p -> p -> p -> p -> p -> [(p, Pair p)]
rkf24 f u0 x0 xn h0 eps = reverse $ rkf24r f u0 x0 xn h0 eps


rk4r :: (Fractional a, Ord a, Enum a) => Pair (a -> Pair a -> a) -> Pair a -> a -> a -> a -> [(a, Pair a)]
rk4r f u0 x0 xn h =
    let grid = takeWhile (\x -> x <= xn) [ x | i <- [0..], let x = x0+h*i ]
    
        fill _ [] acc = acc
        fill u (x:xs) acc = 
            let u' = rk f h x u
            in fill u' xs $ (x,u):acc

    in fill u0 grid []

rkf24r :: (Floating p, Ord p) => Pair (p -> Pair p -> p) -> Pair p -> p -> p -> p -> p -> [(p, Pair p)]
rkf24r f u0 x0 xn h0 eps =
    let fac = 0.8
        facMin = 0.4
        facMax = 4.0
        facMaxBad = 1.0

        fill x u h table stepFacMax = 
            if x >= xn then table
            else 
                let h' = if x + h > xn then xn - x else h
                    (y, Pair e e') = egorov f h' x u
                    err = sqrt $ e*e + e'*e'

                    hNext = h' * (min stepFacMax $ max facMin $ fac * ((eps/err)**0.2))
                    (x', u', fm') = 
                        if err <= eps then (x + h', y, facMax)
                        else (x, u, facMaxBad)

                    t' = if err <= eps then (x', u'):table else table

                in fill x' u' hNext t' fm'

    in fill x0 u0 h0 [(x0, u0)] facMax


nomials :: Fractional t => Pair (t -> Pair t -> t) -> t -> t -> Pair t -> (Pair t, Pair t, Pair t, Pair t)
nomials f h x u = (k1, k2, k3, k4)
    where h' x' = h*x'
          k1 = h' <$> (\f -> f x u) <$> f
          k2 = h' <$> (\f -> f (x + h/2) (u + k1/2)) <$> f
          k3 = h' <$> (\f -> f (x + h/2) (u + k2/2)) <$> f
          k4 = h' <$> (\f -> f (x + h) (u + k3)) <$> f


rk :: Fractional a => Pair (a -> Pair a -> a) -> a -> a -> Pair a -> Pair a
rk f h x u =
    let (k1, k2, k3, k4) = nomials f h x u
    in u + (k1 + 2*k2 + 2*k3 + k4) / 6


egorov :: Fractional b => Pair (b -> Pair b -> b) -> b -> b -> Pair b -> (Pair b, Pair b)
egorov f h x u =
    let (k1, k2, k3, k4) = nomials f h x u
        u' = u + (k1 + 2*k2 + 2*k3 + k4) / 6
        e = u' - (u + (-k1 + 2*k2 + 2*k3 - k4) / 2)
    in (u', e)