module P2015
    (
        Record(..),
        select,
        isJuanWrapper,
        project,
        isNombre,
        conjunct,
        Query,
        tables,
        foldQ,
        tablesFold,
        r1, r2, lr, q1, q2
    ) where

type Record field value = [(field, value)]

r1 = [("Nombre", "Juan"), ("Edad", "40")]
r2 = [("Nombre", "Pepe"), ("Edad", "28"), ("DNI", "123456")]
lr = [r1, r2]


select :: (Record f v -> Bool) -> [Record f v] -> [Record f v]
select f [] = []
select f (r:rs) = if f r then r:(select f rs) else select f rs

isJuanWrapper :: Record [Char] [Char] -> Bool
isJuanWrapper [] = False
isJuanWrapper (fv:fvs) = isJuan fv || isJuanWrapper fvs

isJuan :: ([Char], [Char]) -> Bool

isJuan (f,v) = f == "Nombre" && v == "Juan"

project :: (f -> Bool) -> [Record f v] -> [Record f v]
--project p [] = []
--project p (r:rs) = (convertimeRecord p r):(project p rs)
project p recs = map (\r -> filter (\(k,v) -> p k) r) recs

convertimeRecord :: (f -> Bool) -> (Record f v) -> (Record f v)
convertimeRecord p [] = []
convertimeRecord p ((f,v):fvs) = if p f then (f,v):(convertimeRecord p fvs) else convertimeRecord p fvs

isNombre :: [Char] -> Bool
isNombre x = x == "Nombre"

conjunct :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
conjunct f g x = f x && g x
--conjunct f g = h where h x = f x && g x

data Query field value =
    Table [Record field value] |
    Projection (field -> Bool) (Query field value) |
    Selection ((Record field value) -> Bool) (Query field value) |
    Product (Query field value) (Query field value)

q1 = Projection (/= "job")
    (Projection (/= "age") (Selection (all (\(c,v) -> c /= "name" || v == "Edward Snowden"))
    (Table [ [("name", "Edward Snowden"), ("age", "29"), ("job", "spy")],
             [("name", "Jason Bourne"), ("age", "40"), ("job", "movie spy")]
           ])))

q2 = Projection (conjunct (/= "age") (/= "job"))
        (Selection (all (\(c,v) -> c /= "name" || v == "Edward Snowden"))
        (Table [ [("name", "Edward Snowden"), ("age", "29"), ("job", "spy")],
                 [("name", "Jason Bourne"), ("age", "40"), ("job", "movie spy")]
               ]))

tables :: Query f v -> [[Record f v]]
tables (Table recs) = [recs]
tables (Projection p q) = tables q
tables (Selection p q) = tables q
tables (Product q1 q2) = (tables q1) ++ (tables q2)

foldQ :: ([Record f v] -> b) -> ((f -> Bool) -> b -> b) -> ((Record f v -> Bool) -> b -> b) -> (b -> b -> b) -> Query f v -> b
foldQ f g h i (Table recs) = f recs
foldQ f g h i (Projection p q) = g p (foldQ f g h i q)
foldQ f g h i (Selection p q) = h p (foldQ f g h i q)
foldQ f g h i (Product q1 q2) = i (foldQ f g h i q1) (foldQ f g h i q2)

tablesFold = foldQ (\recs -> [recs]) (\p ans -> ans) (\p ans -> ans) (\ans1 ans2 -> ans1 ++ ans2)
