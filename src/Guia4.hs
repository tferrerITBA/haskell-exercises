module Guia4
    (
        recipe,
        flipp,
        flip2,
        division
    ) where

        recipe x | x > 0 = 1/x
        flipp f x y = f y x
        flip2 f = g where g x y = f y x
        division x y = x / y
        porLaMitad = flipp division 2
