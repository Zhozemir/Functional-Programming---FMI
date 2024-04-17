type Point = (Double, Double)

line :: Point -> Point -> (Double -> Double)
line (x1, y1) (x2, y2)
 | x1 == x2 = error "x1 and x2 have to be not-equal"
line (x1, y1) (x2, y2) = \x -> y1 + (x - x1) * (y2 - y1) / (x2 - x1)

liesOn :: (Double -> Double) -> (Point -> Bool)
liesOn f = (\(x,y) -> y == f x)

main :: IO()
main = do

 print $ line (1, 2) (3, 4) 5.5 == 6.5 -- my test

 print $ liesOn (line (1, 2) (3, 4)) (5.5, (line (1, 2) (3, 4)) 5.5) == True --myTest