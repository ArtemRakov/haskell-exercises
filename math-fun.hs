-- fibonacci n = fibonacciHelper 0 n

-- fibonacciHelper f_acc s_acc 0 n = 0
-- fibonacciHelper f_acc s_acc 1 n = 1
fibonacciHelper f_acc s_acc count n | count == n = f_acc
                                    | n < 0 = fibonacciHelper s_acc (f_acc - s_acc) (count - 1) n
                                    | n > 0 = fibonacciHelper s_acc (f_acc + s_acc) (count + 1) n

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacciHelper 0 1 0 n


seqA :: Integer -> Integer
seqA n | n == 0 = 1
       | n == 1 = 2
       | n == 2 = 3
       | n >= 3 = let
         helper a1 a2 a3 count n | count == n = a1
                                 | otherwise = helper a2 a3 (a3 + a2 - 2 * a1) (count + 1) n
          in helper 1 2 3 0 n


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = helper 0 0 (abs x) where
    helper 0 0 0 = (0, 1)
    helper sum number_count number | number == 0 = (sum, number_count)
                                   | otherwise = helper (sum + (mod number 10)) (number_count + 1) (div number 10)

integration :: (Double -> Double) -> Double -> Double -> Double
-- integration f a b = let
--       n = 1000
--       h = (b - a) / n
--         in h * (((f a + f b) / 2) + helper f h 0 0 1)


-- helper f h acc temp n | n == 1000 = acc
--                  | otherwise = helper f h (acc + f(temp + h)) (temp + h) (n + 1)



integration f a b = h * ((f a + f b) / 2 + helper 1 0) where
  n = 1000
  h = (b - a) / n
  helper i acc | i == n = acc
               | otherwise = helper (i + 1) (acc + f (a + (h * i)))
