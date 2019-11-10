module Guia3
    (
        curryZ,
        uncurryZ,
        appDup,
        appFork,
        appPar,
        subst3,
        compose,
        timesTwoPlusThree,
        fourTimes,
        --manyy
    ) where

        curryZ f = g where g x y = f(x,y)
        uncurryZ f = g where g(x,y) = f x y
        appDup f = \x -> f (x,x)
        appFork (f,g) = \x -> (f x, g x)
        appPar (f, g) = \(x,y) -> (f x, g y)
        substOrig f = h where  h g = k where k x = (f x) (g x)
        subst3 f g = \x -> (f x) (g x)
        compose = \f -> (\g -> (\x -> f (g x)))
        --compose f g x = f(g x)
        --manyy 0 f x = x
        --manyy n f x = compose f (many n-1 f x) x
        suma x y = x + y
        doble x = x + x
        timesTwoPlusThree x = compose (suma 3) doble x
        fourTimes f x = compose f (compose f (compose f f)) x
        -- many 0 f x = x
        -- many n f x = f (many (n-1) f x)
