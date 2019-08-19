module Example
    ( subst
    ) where

-- f :: Int -> Bool
-- g :: Char -> Int
-- compose x y = (\z -> x(y z))
-- f x = True
-- g y = 1
-- a = compose f g

subst f g x = f x (g x)
