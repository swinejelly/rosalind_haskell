replace :: (Eq a) => a -> a -> [a] -> [a]
replace orig new (x:xs) =
  if orig == x
  then new:(replace orig new xs)
  else x:(replace orig new xs)
replace _ _ _ = []

dnaToRna :: String -> String
dnaToRna = replace 'T' 'U'

main = do
  input <- getLine
  putStrLn (dnaToRna input)
