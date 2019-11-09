module Guia3
    (
        curryZ,
        uncurryZ,
        appDup,
        appFork
    ) where

        curryZ f = g where g x y = f(x,y)
        uncurryZ f = g where g(x,y) = f x y
        appDup f = \x -> f (x,x)
        appFork (f,g) = \x -> (f x, g x)
