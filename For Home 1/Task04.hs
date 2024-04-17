sumCubesPow :: Int -> Int -> Int
sumCubesPow n k = n^3 + k^3

sumCubesNoPow :: Int -> Int -> Int
sumCubesNoPow n k = n*n*n + k*k*k

main :: IO()
main = do
 
    print $ sumCubesPow 5 1 == 126
    print $ sumCubesPow 10 50 == 126000
    print $ sumCubesPow 3 4 == 91 -- my test
    
    print $ sumCubesNoPow 5 1 == 126
    print $ sumCubesNoPow 10 50 == 126000
    print $ sumCubesNoPow 9 3 == 756 -- my test