import Data.List
import Data.Function
import Data.Array
import Control.Arrow

data Code = N Int | S Code | Code :+ Code | Code :* Code | Code :^ Code | PP Code | PPP Code deriving Show

size = snd . (churches!)

encode 1 = (N 1, 1)
encode 2 = (S (N 1), 10 + size 1)

encode 4 = (PP (N 2), 6 + size 2)
encode 27 = (PP (N 3), 6 + size 3)

encode 16 = (PPP (N 2), 9 + snd (encode 2))

encode 8 = (N 2 :^ N 3, size 2 + size 3 + 1)
encode 25 = (N 5 :^ N 2, size 5 + size 2 + 1)
encode 36 = (N 6 :^ N 2, size 6 + size 2 + 1)
encode 64 = (N 4 :^ N 3, size 4 + size 3 + 1)
encode 81 = (N 3 :^ N 4, size 3 + size 4 + 1)
encode 100 = (N 10 :^ N 2, size 10 + size 2 + 1)
encode 121 = (N 11 :^ N 2, size 11 + size 2 + 1)

encode n = minimumBy (compare `on` snd) $ suc : adds ++ muls
    where
        suc = (S (N (n - 1)), 10 + size (n - 1))
        adds = [(N m :+ N (n - m), 11 + size m + size (n - m)) | m <- [1..n-1]]
        muls = [(N m :* N (n `div` m), 5 + size m + size (n `div` m)) | m <- [2..n `div` 2], mod n m == 0]

churches = listArray (1, 256) $ map encode [1..] 

main = print $ map (second fst) $ assocs churches