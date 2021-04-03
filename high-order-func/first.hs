import Data.Char

readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
-- filterDisj _ _ [] = []
-- filterDisj f1 f2 (x:xs) | f1 x = x : filterDisj f1 f2 xs
--   | f2 x = x : filterDisj f1 f2 xs
--   | otherwise = filterDisj f1 f2 xs
filterDisj f1 f2 = filter $ \x -> f1 x || f2 x

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (filter (<= x) xs) ++ [x] ++ qsort (filter (> x) xs)

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap $ \x -> [x^2, x^3]

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems [x] = [[x]]
groupElems (x:xs) | head xs == x = let (y:ys) = groupElems xs in (x : y) : ys
  | otherwise = [x] : groupElems xs

-- perms :: [a] -> [[a]]
-- perms [] = [[]]
-- perms xs = helper xs [] where
--   helper [] acc = []
--   helper (x:xs) acc = concat([[x], acc, xs]) : concat([[x], reverse xs, reverse acc]) : helper xs (x : acc)
  -- helper (x:xs) acc = (concatMap $ \x -> [[x], acc, xs], [[x], reverse xs, reverse acc] xs) ++ helper xs (x : acc)
