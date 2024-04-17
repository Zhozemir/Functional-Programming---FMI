isPresentRecNonPM :: Int -> [Int] -> Bool
isPresentRecNonPM x xs
 | null xs = False
 | x == head xs = True
 | otherwise = isPresentRecNonPM x (tail xs)

isPresentRecPM :: Int -> [Int] -> Bool
isPresentRecPM _ [] = False
isPresentRecPM x (y:ys)
 | x == y    = True
 | otherwise = isPresentRecPM x ys

isPresentFunc :: Int -> [Int] -> Bool
isPresentFunc x xs = elem x xs
 
main :: IO ()
main = do

 print $ isPresentRecNonPM 0 [] == False
 print $ isPresentRecNonPM 0 [1, 2, 3] == False
 print $ isPresentRecNonPM 0 [0, -1, 2] == True
 print $ isPresentRecNonPM 3 [4, 3, 5] == True -- my test
 
 print $ isPresentRecPM 0 [] == False
 print $ isPresentRecPM 0 [1, 2, 3] == False
 print $ isPresentRecPM 0 [0, -1, 2] == True
 print $ isPresentRecPM 4 [3, 1, 6] == False -- my test
 
 print $ isPresentFunc 0 [] == False
 print $ isPresentFunc 0 [1, 2, 3] == False
 print $ isPresentFunc 0 [0, -1, 2] == True
 print $ isPresentFunc 5 [4, 5, 3] == True -- my test
 