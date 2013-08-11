import Data.List (lookup, reverse)

replaceOccurrences :: (Eq a) => [(a, a)] -> [a] -> [a]
replaceOccurrences replacements (x:xs) =
  case (lookup x replacements) of
    Just r -> r:(replaceOccurrences replacements xs)
    Nothing -> x:(replaceOccurrences replacements xs)
replaceOccurrences _ _ = []

reverseComplement :: String -> String
reverseComplement = 
  reverse . replaceOccurrences 
    [('A', 'T'),
     ('T', 'A'),
     ('G', 'C'),
     ('C', 'G')]

main = do
  inputStr <- getLine
  putStrLn (reverseComplement inputStr)
