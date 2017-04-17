import Control.Monad
import Control.Monad.Amb

multipleDwelling :: Amb r [(String,Int)]
multipleDwelling = do
    [b,c,f,m,s]  <- aPermutationOf [1..5]
    guard (b /= 5)
    guard (c /= 1)
    guard (f /= 1 && f /= 5)
    guard (m > c)
    guard (abs (s - f) /= 1)
    guard (abs (f - c) /= 1)
    return $ zip ["baker","cooper","fletcher","miller","smith"] [b,c,f,m,s]

main :: IO ()
main = do
  print $ allValues multipleDwelling
