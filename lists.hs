fibStream :: [Integer]
fibStream = zipWith (+) (0:fibStream) (0:1:fibStream)
