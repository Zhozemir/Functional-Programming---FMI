import Data.List

repeater :: String -> Int -> String -> String
repeater str count glue = intercalate glue (replicate count str)

main :: IO ()
main = do 

 print $ (repeater "I love Haskell") 3 " " == "I love Haskell I love Haskell I love Haskell"
 print $ (repeater "Quack") 5 "!" == "Quack!Quack!Quack!Quack!Quack"
 print $ (repeater "Ball") 3 "*" == "Ball*Ball*Ball" -- my test