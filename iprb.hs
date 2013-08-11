dominantProbability :: (Int, Int, Int) -> Double
dominantProbability (k, m, n) = 
  let k' = (fromIntegral k) :: Double
      m' = (fromIntegral m) :: Double
      n' = (fromIntegral n) :: Double
      total = k' + m' + n'
  in
     
