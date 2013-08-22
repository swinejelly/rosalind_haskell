main = do
  txt <- readFile "iev.txt"
  let (nAAAA:nAAAa:nAAaa:nAaAa:nAaaa:naaaa:_) = map read $ words txt
      numDom =
        2 * nAAAA +
        2 * nAAAa +
        2 * nAAaa +
        2 * 0.75 * nAaAa +
        2 * 0.50 * nAaaa +
        2 * 0 * naaaa
  putStr (show numDom)


