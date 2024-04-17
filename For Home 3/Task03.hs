calcSeriesSum :: Double -> Double -> Double
calcSeriesSum x n = helper (n+1) 3 0 (-2) 1
 where 
  helper 0 devisor result element power = result
  helper n devisor result element power = helper (n-1) (devisor+2) (result + element) ( (element * (-2) * x ^ power) / devisor) (power + 1)

main :: IO()
main = do
    
 print $ calcSeriesSum 1 0 == -2.0 
 print $ calcSeriesSum 1 1 == -0.6666666666666667
 print $ calcSeriesSum 1 2 == -1.2000000000000002
 print $ calcSeriesSum 1 3 == -1.047619047619048
 print $ calcSeriesSum 1 4 == -1.0814814814814817
 print $ calcSeriesSum 1 5 == -1.0753246753246755
 print $ calcSeriesSum 1 6 == -1.0762718762718764
 print $ calcSeriesSum 3 5 == 86442.58181818182 -- my test