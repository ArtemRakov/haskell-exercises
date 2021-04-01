module Demo where

import Prelude hiding (zip, unzip)

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x, y) : zip xs ys

unzip :: [(a,b)] -> ([a], [b])
unzip [] = ([],[])
unzip ((x,y):xys) =
  let (xs, ys) = unzip xys
  in (x:xs, y:ys)

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 [] [] [] = []
sum3 [] ys zs = sum3 [0] ys zs
sum3 xs [] zs = sum3 xs [0] zs
sum3 xs ys [] = sum3 xs ys [0]
sum3 (x:xs) (y:ys) (z:zs) = (x + y + z) : sum3 xs ys zs


-- groupElems :: Eq a => [a] -> [[a]]
-- groupElems [] = []
-- groupElems [x] = [[x]]
-- groupElems (x:xs) | x == head xs = let (y:ys) = groupElems xs in (x : y) : ys
--                   | otherwise = [x] : groupElems xs
--
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems (x:xs) = helper xs [x] where
  helper [] acc = [acc]
  helper (x:xs) acc | x == head acc = helper xs (x : acc)
                    | otherwise = acc : helper xs [x]
