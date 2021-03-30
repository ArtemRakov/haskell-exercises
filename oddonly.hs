oddOnly :: Integral a => [a] -> [a]
oddOnly [] = []
oddOnly (x : xs)
  | odd x = x : oddOnly xs
  | otherwise = oddOnly xs

