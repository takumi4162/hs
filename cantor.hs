-- cantor function (iterative definition) --
cantor :: Double -> Int -> Double
cantor x 0 = x
cantor x n  
  | 0 <= x && x <= 1/3 = 1/2 * (cantor (3*x) (n-1))
  | 1/3 <= x && x <= 2/3 = 1/2
  | 2/3 <= x && x <= 1 = 1/2 + 1/2 * (cantor (3*x-2) (n-1))
  | otherwise = x

main = do 
  let n = 100   -- precision of cantor function
  x <- getLine
  print $ cantor (read x :: Double) n
