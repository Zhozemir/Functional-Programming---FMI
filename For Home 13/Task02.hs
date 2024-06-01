import Data.List

type Misses = Int
type Lates = Int
type Criterion = (Misses, Lates)

data Attendance = Absent | Late | Present 
 deriving (Eq, Show)
type StudentRecord = [Attendance]

maxConsecutiveLates :: StudentRecord -> Int
maxConsecutiveLates = maximum . (0:) . map length . filter ((== Late) . head) . group

countAbsents :: StudentRecord -> Int
countAbsents = length . filter (== Absent)

canPass :: Criterion -> (StudentRecord -> Bool)
canPass (maxMisses, maxLates) = 
    \record -> countAbsents record <= maxMisses && maxConsecutiveLates record <= maxLates

cP = canPass (1, 2)

main :: IO ()
main = do
 
 print $ cP [Present, Late, Present, Absent, Present, Present, Present, Absent] == False
 print $ cP [Present, Late, Present, Late, Present, Late, Present, Absent, Late, Present] == True
 print $ cP [Present, Late, Present, Late, Late, Late, Present, Present, Absent, Present] == False
