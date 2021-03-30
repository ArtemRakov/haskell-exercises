nTimes :: a -> Int -> [a]
nTimes element times = helper element times [] where
  helper element 0 acc = acc
  helper element times acc = helper element (times - 1) (element : acc)

