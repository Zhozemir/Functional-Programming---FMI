type Rat a = (a, a)

normalize :: (Integral a) => Rat a -> Rat a
normalize (x, y) = (div x gcdXy, div y gcdXy)
 where
  gcdXy = gcd x y

sumRats :: (Integral a) => Rat a -> Rat a -> Rat a
sumRats (x1, y1) (x2, y2) = normalize (x1 * y2 + x2 * y1, y1 * y2)

multiplyRats :: (Integral a) => Rat a -> Rat a -> Rat a
multiplyRats (x1, y1) (x2, y2) = normalize (x1 * x2, y1 * y2)

divideRats :: (Integral a) => Rat a -> Rat a -> Rat a
divideRats (x1, y1) (x2, y2) = normalize (x1 * y2, y1 * x2)

areEqual :: (Integral a) => Rat a -> Rat a -> Bool
areEqual (x1, y1) (x2, y2) = x1 * y2 == x2 * y1

main :: IO()
main = do
 print $ sumRats (2, 5) (5, 2) == (29, 10)
 print $ sumRats (52, 123) (96, 14) == (6268, 861)
 print $ sumRats (2, 5) (3, 5) == (1, 1)
 print $ sumRats (3 ,4) (1, 3) == (13, 12) -- my test

 print $ multiplyRats (2, 5) (5, 2) == (1, 1)
 print $ multiplyRats (52, 123) (96, 14) == (832, 287)
 print $ multiplyRats (2, 5) (3, 5) == (6, 25)
 print $ multiplyRats (1, 3) (4, 7) == (4, 21) -- my test

 print $ divideRats (2, 5) (5, 2) == (4, 25)
 print $ divideRats (52, 123) (96, 14) == (91, 1476)
 print $ divideRats (2, 5) (3, 5) == (2, 3)
 print $ divideRats (6, 1) (3, 4) == (8 ,1) -- my test

 print $ areEqual (2, 5) (5, 2) == False
 print $ areEqual (52, 123) (52 * 3, 123 * 3) == True
 print $ areEqual (2, 6) (5, 15) == True
 print $ areEqual (3, 4) (6, 7) == False -- my test