import Data.List

data Point = TwoD Double Double | ThreeD Double Double Double
  deriving (Show, Eq)

distance :: Point -> Point -> Double
distance (TwoD x1 y1) (TwoD x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
distance (ThreeD x1 y1 z1) (ThreeD x2 y2 z2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2 + (z2 - z1)^2)
distance _ _ = error "Mixed dimensions are not allowed"

getClosestDistance :: [Point] -> (Double, Point, Point)
getClosestDistance [] = error "Empty list of points"
getClosestDistance [_] = error "Only one point given"
getClosestDistance points = foldl1 minDistance pairs
  where
    pairs = [(distance p1 p2, p1, p2) | (p1:ps) <- tails points, p2 <- ps]
    minDistance acc@(d1, _, _) current@(d2, _, _) = if d2 < d1 then current else acc

main :: IO ()
main = do

  print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
  print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2)] == (4.47213595499958,ThreeD 4.0 5.0 6.0,ThreeD 2.0 5.0 10.0)
  print $ getClosestDistance [(ThreeD 4 5 6), (ThreeD 2 5 10), (ThreeD 5 2 (-10)), (ThreeD (-2) 1 45), (ThreeD 12 0 2), (ThreeD 6 5 4)] == (2.8284271247461903,ThreeD 4.0 5.0 6.0,ThreeD 6.0 5.0 4.0)
  print $ getClosestDistance [(TwoD 4 6), (TwoD 5 10), (TwoD 5 29), (TwoD 1 45), (TwoD 0 2), (TwoD 69 42)] == (4.123105625617661,TwoD 4.0 6.0,TwoD 5.0 10.0)
  print $ getClosestDistance [(TwoD 0 0), (TwoD 3 4)] == (5.0, TwoD 0.0 0.0, TwoD 3.0 4.0) 
