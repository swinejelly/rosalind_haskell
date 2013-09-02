import Network.HTTP
import Data.List (intercalate)
import Control.Exception (catch)

fastaURL s = "http://www.uniprot.org/uniprot/" ++ s ++ ".fasta"

uniprotURL = "http://www.uniprot.org"

normalizeFastaURL s@('/':_) = uniprotURL ++ s
normalizeFastaURL s = s

getFastaContent :: String -> String
getFastaContent = foldl (++) "" . drop 1 . lines

motifMatches :: String -> [Bool]
motifMatches s@(a:b:c:d:_) = (a == 'N' && b /= 'P' && (c == 'S' || c == 'T') && d /= 'P') 
                            : (motifMatches $ tail s)
motifMatches _ = []

matchIndices :: String -> [Int]
matchIndices s = map fst $ filter snd $
  zip [1..] matches
  where matches = motifMatches s

getContent :: String -> IO String
getContent s = do
  result <- simpleHTTP (getRequest s)
  case result of
    Right response ->
      case findHeader HdrLocation response of
        Just l -> getContent $ normalizeFastaURL l
        Nothing -> getResponseBody result
    _ -> error "Connection error"

main = do
  inpStr <- readFile "mprt.txt"
  let names = lines inpStr
      urls = map fastaURL names
      fastasIO = sequence (map getContent urls)
  fastas <- fastasIO
  let fastaMatches = filter (not . null . snd) $ 
          zip names (map (matchIndices . getFastaContent) fastas)
      outputIOs = map formatMatch fastaMatches
        where formatMatch (name, indices) =
                do putStrLn name
                   putStrLn indices'
                  where indices' = intercalate " " (map show indices)
  sequence_ outputIOs
