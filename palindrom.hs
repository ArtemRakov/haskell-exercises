isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = helper list [] where
  helper [] acc = acc == list
  helper (x:xs) acc = helper xs (x:acc)
