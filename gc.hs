import Data.List.Split (splitOn)
import Data.List (findIndices, maximumBy)
import System.IO

data Fasta = Fasta String String
  deriving (Read,Show)

getName :: Fasta -> String
getName (Fasta n _) = n

getSequence :: Fasta -> String
getSequence (Fasta _ s) = s

getContentGC :: Fasta -> Double
getContentGC f = 
  let sequence = getSequence f
      size = (fromIntegral $ length sequence) :: Double
      gc = (fromIntegral $ length (findIndices 
       (\c -> c == 'G' || c == 'C') sequence)) :: Double
  in gc / size
      

-- Reads a sequence of fastas from a fasta format string
readFastas :: String -> [Fasta]
readFastas s = 
  let entries = tail $ splitOn ">" s :: [String]
  in map entryToFasta entries
  where entryToFasta :: String -> Fasta
        entryToFasta s = 
          let name = (head $ lines s) --tail removes the ">"
              sequence = concat $ (tail $ lines s)
          in Fasta name sequence

main = do
  inpStr <- readFile "gc.txt"
  let fastas = readFastas inpStr
      maxFasta = maximumBy 
                   (\a b -> (getContentGC a) `compare` (getContentGC b))
                   fastas
  putStrLn (getName maxFasta)
  putStrLn (show ((getContentGC maxFasta) * 100))
