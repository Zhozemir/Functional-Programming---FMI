type Name = String
type Capital = Name 
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

capitalTemperature :: Capital -> [City] -> AvgYearlyTemperature
capitalTemperature capital = avgTemp . filter isCapital
  where
    isCapital (City name _ _) = name == capital
    avgTemp cities = sum (map (\(City _ _ temp) -> temp) cities) / fromIntegral (length cities)

coldestCapital :: [Country] -> Name
coldestCapital = fst . foldl1 colder . map countryCapitalTemp
  where
    countryCapitalTemp (Country name capital cities) = (name, capitalTemperature capital cities)
    colder a@(n1, t1) b@(n2, t2)
      | t1 < t2   = a
      | otherwise = b

main :: IO ()
main = do
    
  print $ coldestCapital [Country "Bulgaria" "Sofia" [City "Varna" 0 16, City "Plovdiv" 120 14, City "Sofia" 420 13],
                          Country "Germany" "Berlin" [City "Munchen" 200 15, City "Berlin" 150 12, City "Ulm" 210 15],
                          Country "France" "Paris" [City "Paris" 180 15, City "Nice" 0 14, City "Lyon" 500 13]] == "Germany"
                          
  print $ coldestCapital [Country "Bulgaria" "Sofia" [City "Varna" 0 (-16), City "Plovdiv" 120 34, City "Sofia" 420 13],
                          Country "Germany" "Berlin" [City "Munchen" 200 (-15), City "Berlin" 150 12, City "Ulm" 210 15],
                          Country "France" "Paris" [City "Paris" 180 15, City "Nice" 0 14, City "Lyon" 500 13]] == "Germany"

  print $ coldestCapital [Country "Norway" "Oslo" [City "Oslo" 150 (-2), City "Bergen" 20 1, City "Trondheim" 130 0],
                          Country "Sweden" "Stockholm" [City "Stockholm" 100 1, City "Gothenburg" 0 2, City "Malmo" 10 3]] == "Norway" -- my test
