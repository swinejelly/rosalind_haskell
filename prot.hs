import qualified Data.Map as Map
import Data.List.Split (splitEvery)

parseMap :: String -> Map.Map String String
parseMap s = Map.fromList table
  where table = (map (\[c, p] -> (c, p)) (splitEvery 2 (words s)))

rnaToProt :: Map.Map String String -> String -> String
rnaToProt m rna =
  let codons = splitEvery 3 rna
      convert r = case (Map.lookup r m) of
        Just "Stop" -> ""
	Just a -> a
	Nothing -> ""
  in concat $ map convert codons

main = do
  inpStr <- readFile "prot.txt"
  tableStr <- readFile "codontable.table"
  let codonMap = parseMap tableStr
  putStrLn (rnaToProt codonMap inpStr)
