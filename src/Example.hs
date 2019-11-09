module Example
    ( subst,
      compose,
      constante,
      h,
      lala
    ) where

import Data.Char

lala x = digitToInt x

-- f :: Int -> Bool
-- g :: Char -> Int
compose x y = (\z -> x(y z))
f x = True
g y = 1
a = compose f g
--h::Char->Integer
h 'a' = 1

subst f g x = f x (g x)

constante x y = x
-- p1 -> p2 -> p1
