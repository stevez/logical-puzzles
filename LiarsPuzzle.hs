import Control.Monad
import Control.Monad.Amb
import Data.List

liarsPuzzle :: Amb r [(String,Int)]
liarsPuzzle = do
    [b,e,j,k,m] <- aPermutationOf [1..5]
    guard (xor (k == 2) (b == 3))
    guard (xor (e == 1) (j == 2))
    guard (xor (j == 3) (e == 5))
    guard (xor (k == 2) (m == 4))
    guard (xor (m == 4) (b == 1))
    return $ sortOn snd $ zip ["Betty", "Ethel","Joan","Kitty","Marry"] [b,e,j,k,m]


xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _  = False

main :: IO ()
main = do
  print $ allValues liarsPuzzle
