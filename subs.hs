import Data.List

subIndices :: (Eq a) => [a] -> [a] -> [Int]
subIndices sub body = findIndices (sub `isPrefixOf`) (tails body)

main = do
  body <- getLine
  sub <- getLine
  let indices = subIndices sub body
      output = intercalate " " (map (\x -> show (x + 1)) indices)
  putStr output
