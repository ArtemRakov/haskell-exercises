class Printable a where
  toString :: a -> [Char]


instance Printable Bool where
  toString True = "true"
  toString False = "false"

instance Printable () where
  toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool
    doesEnrageGork _ = True

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool
    doesEnrageMork _ = True

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | doesEnrageMork a == True = stomp a
                  | doesEnrageGork a == True = stab a
                  | doesEnrageGork a && doesEnrageMork a == True = (stomp . stab) a
                  | otherwise = a

class (Eq a, Enum a, Bounded a) => SafeEnum a where
  ssucc :: a -> a
  ssucc val | maxBound == val = minBound
            | otherwise = succ val

  spred :: a -> a
  spred val | minBound == val = maxBound
            | otherwise = pred val

instance SafeEnum Bool

avg :: Int -> Int -> Int -> Double
avg a b c = fromIntegral(a + b + c) / 3
