import Control.Monad
import Control.Monad.Amb
import Data.List

eightQueens :: Amb r [Int]
eightQueens = do
   l <- aPermutationOf [1..8]
   let pairs = filter ((==2) . length) $ subsequences $ zip [1..8] l
   when (any sameDiag pairs) empty
   return l


pairs :: [(Int,Int)] -> [[(Int,Int)]]
pairs = filter ((==2) . length) . subsequences

sameDiag :: [(Int,Int)] -> Bool
sameDiag [(x1,y1),(x2,y2)] = abs (x1 - x2) == abs (y1 - y2)

main :: IO ()
main = do
  let result = allValues  eightQueens
  putStrLn "Solutions:"
  print result
  putStrLn ( "Found solutions:" ++ show (length result) )
