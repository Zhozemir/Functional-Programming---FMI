type Cylinder = (Double, Double)

getVolume :: Cylinder -> Double 
getVolume (radius, height) = 3.14 * radius * radius * height

getVolumes :: [Cylinder] -> [Double]
getVolumes cylinders = map getVolume cylinders

main :: IO()
main = do
    print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] 
