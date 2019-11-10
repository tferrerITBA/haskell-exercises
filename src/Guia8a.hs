module Guia8a
    (
        len,
        suma,
        multi,
        append,
        concatenate,
        element,
        todos,
        algun,
        subset,
        reverso,
        zipp,
        unzipp
    ) where

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

multi :: [Int] -> Int
multi [] = 1
multi (x:xs) = x * multi xs

append [] ys = ys
append (x:xs) ys = x:append xs ys

concatenate :: [[a]] -> [a]
concatenate [] = []
concatenate (xs:xss) = append xs (concatenate xss)

element :: Eq a => a -> [a] -> Bool
element a [] = False
element a (x:xs) = x == a || element a xs

todos :: (a -> Bool) -> [a] -> Bool
todos f [] = True
todos f (x:xs) = f x && todos f xs

algun :: (a -> Bool) -> [a] -> Bool
algun f [] = True
algun f (x:xs) = f x || algun f xs

count :: (a -> Bool) -> [a] -> Int
count f [] = 0
count f (x:xs) = if f x then 1 + count f xs else count f xs

subset :: Eq a => [a] -> [a] -> Bool
subset [] _ = True
subset (x:xs) l2 = element x l2 && subset xs l2

reverso :: [a] -> [a]
reverso [] = []
reverso (x:xs) = append (reverso xs) [x]

zipp :: [a] -> [b] -> [(a,b)]
zipp [] _ = []
zipp _ [] = []
zipp (x:xs) (y:ys) = (x,y) : (zipp xs ys)

unzipp :: [(a,b)] -> ([a], [b])
unzipp [] = ([],[])
unzipp ((a,b):abz) = let (j,k) = unzipp abz in (a:j, b:k)
