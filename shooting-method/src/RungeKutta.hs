module RungeKutta
    ( rk4
    , rkf24
    , rk4r
    , rkf24r
    ) where

import Pair (Pair(..))

-- | Решает систему с правыми частями 'f', начальными условиями 'u0' на отрезке ['x0','xn']
-- методом Рунге-Кутты 4 порядка точности с постоянным шагом 'h'.
--
-- Возвращает список кортежей @(x, Pair u u')@.
rk4 :: (Fractional a, Ord a, Enum a) => Pair (a -> Pair a -> a) -> Pair a -> a -> a -> a -> [(a, Pair a)]
rk4 f u0 x0 xn h = reverse $ rk4r f u0 x0 xn h

-- | Решает систему с правыми частями 'f', начальными условиями 'u0' на отрезке ['x0','xn']
-- методом Рунге-Кутты 4 порядка точности с автоматическим выбором шага с точностью 'eps'.
--
-- Возвращает список кортежей @(x, Pair u u')@.
rkf24 :: (Floating p, Ord p) => Pair (p -> Pair p -> p) -> Pair p -> p -> p -> p -> p -> [(p, Pair p)]
rkf24 f u0 x0 xn h0 eps = reverse $ rkf24r f u0 x0 xn h0 eps

-- | Как 'rk4', но таблица значений возвращается по убыванию 'x'.
rk4r :: (Fractional a, Ord a, Enum a) => Pair (a -> Pair a -> a) -> Pair a -> a -> a -> a -> [(a, Pair a)]
rk4r f u0 x0 xn h =
    let grid = takeWhile (\x -> x <= xn) [ if x < xn then x else x - offset | i <- [0..], let x = x0+h*i ] 
            where offset = 0.00001
    
        fill _ [] acc = acc
        fill u (x:xs) acc = 
            let u' = rk f h x u
            in fill u' xs $ (x,u):acc

    in fill u0 grid []

-- | Как 'rkf24', но таблица значений возвращается по убыванию 'x'.
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

-- | Вычисляет 'k1', 'k2', 'k3', 'k4' для столбца функций 'f', текущей точки 'x' и значениях 'u' в предыдущей точке.
nomials :: Fractional t => Pair (t -> Pair t -> t) -> t -> t -> Pair t -> (Pair t, Pair t, Pair t, Pair t)
nomials f h x u = (k1, k2, k3, k4)
    where h' x' = h*x'
          k1 = h' <$> (\f -> f x u) <$> f
          k2 = h' <$> (\f -> f (x + h/2) (u + k1/2)) <$> f
          k3 = h' <$> (\f -> f (x + h/2) (u + k2/2)) <$> f
          k4 = h' <$> (\f -> f (x + h) (u + k3)) <$> f

-- | Вычисляет значение в точке @u+1@ по формуле для метода Рунге-Кутты 4 порядка точности.
rk :: Fractional a => Pair (a -> Pair a -> a) -> a -> a -> Pair a -> Pair a
rk f h x u =
    let (k1, k2, k3, k4) = nomials f h x u
    in u + (k1 + 2*k2 + 2*k3 + k4) / 6

-- | Вычисляет контрольный член Егорова.
egorov :: Fractional b => Pair (b -> Pair b -> b) -> b -> b -> Pair b -> (Pair b, Pair b)
egorov f h x u =
    let (k1, k2, k3, k4) = nomials f h x u
        u' = u + (k1 + 2*k2 + 2*k3 + k4) / 6
        e = u' - (u + (-k1 + 2*k2 + 2*k3 - k4) / 2)
    in (u', e)