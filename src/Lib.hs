module Lib
    ( someFunc,
      seven,
      sign,
      sign_alt,
      abs_mio,
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

seven x = 7
sign x = if x > 0 then 1 else (if x < 0 then -1 else 0)

sign_alt x
  | x < 0 = -1
  | x > 0 = 1
  | otherwise = 0

a = -1 :: Int
-- :{
-- |  let abs n = do {
-- |    n >= 0 = n
-- | otherwise = -n
-- | }
-- | :}
abs_mio n = sign_alt n * n

