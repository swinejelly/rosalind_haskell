import Data.List.Split (splitWhen)
--Get the Nth (starting from 1 as in 1,1,2...) element of the fibonacci sequence
--for k pairs from each reproducing age pair
fibN :: Int -> Int -> Integer
fibN n k = fibs!!n
  where fibs :: [Integer]
        fibs = 0 : 1 : zipWith (\x y -> ((fromIntegral k) * x) + y) fibs (tail fibs)

main = do
  inpStr <- getLine
  let nums = map (read :: String -> Int) (splitWhen (==' ') inpStr)
  putStrLn (show (fibN (nums!!0) (nums!!1)))
