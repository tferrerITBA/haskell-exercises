module Guia2
    (
        first,
        apply,
        twice,
        doble,
        swap,
        uflip
    ) where

        first (x,y) = x
        apply f = g where g x = f x
        twice f = g where g x = f (f x)
        doble x = x + x
        swap (x, y) = (y, x)
        uflip f = g where g p = f (swap p)
