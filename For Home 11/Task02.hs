type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature deriving (Show)
data Country = Country Name Capital [City] deriving (Show)

capitalElevation :: Capital -> [City] -> Elevation
capitalElevation capitalName cities = elevation
  where
    (City _ elevation _) = head $ filter (\(City name _ _) -> name == capitalName) cities

highestCapital :: [Country] -> Name
highestCapital countries = countryName bestCountry
  where
    bestCountry = foldr1 highest countries
    highest country1@(Country _ capitalName1 cities1) country2@(Country _ capitalName2 cities2)
      | capitalElevation capitalName1 cities1 > capitalElevation capitalName2 cities2 = country1
      | otherwise = country2
    countryName (Country name _ _) = name

main :: IO ()
main = do
    print $ highestCapital 
        [ (Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)])
        , (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)])
        , (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])
        ] == "Bulgaria"

    print $ highestCapital 
        [ (Country "Italy" "Rome" [(City "Rome" 21 15), (City "Milan" 122 13), (City "Naples" 17 16)])
        , (Country "Switzerland" "Bern" [(City "Zurich" 400 15), (City "Bern" 540 10), (City "Geneva" 375 14)])
        , (Country "Chile" "Santiago" [(City "Santiago" 520 17), (City "Valparaiso" 10 18)])
        ] == "Switzerland" -- my test
