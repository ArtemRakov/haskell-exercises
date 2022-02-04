fibStream :: [Integer]
fibStream = zipWith (+) (0:fibStream) (0:1:fibStream)


data Odd = Odd Integer
  deriving (Eq, Show)

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | even m = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"

instance Enum Odd where
  toEnum i = Odd(toInteger i)
  fromEnum (Odd n) = fromEnum n

  succ (Odd n) = Odd (n+2)
  pred (Odd n) = Odd (n-2)

  enumFrom (Odd n) = map Odd [n,n+2..]
  enumFromTo (Odd n) (Odd m) = map Odd [n,n+2..m]
  enumFromThen (Odd n) (Odd n') = map Odd [n,n'..]
  enumFromThenTo (Odd n) (Odd n') (Odd m) = map Odd [n,n'..m]
