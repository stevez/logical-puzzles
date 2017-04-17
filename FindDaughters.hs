import Control.Monad
import Control.Monad.Amb

findDaughters :: Amb r [(String,String,String)]
findDaughters = do
    [dm,dd,dh,db,dp] <- aPermutationOf ["Mary","Gabrielle","Lorna","Rosalind","Melissa"]

    ym <- aMemberOf ["Lorna"]
    yd <- aMemberOf ["Melissa"]
    yh <- aMemberOf ["Rosalind"]
    yb <- aMemberOf ["Gabrielle"]
    yp <- aMemberOf ["Mary"]

    guard (dm /= ym)
    guard (dd /= yd)
    guard (dh /= yh)
    guard (db /= yb)
    guard (dp /= yp)

    guard (dm == "Mary")
    guard (db == "Melissa")
    guard ( dd == "Gabrielle" && yd == dp
           || dh == "Gabrielle" && yh == dp )

    return $ zip3 ["Moore","Downing","Hall","Banacle","Parker"] [dm,dd,dh,db,dp] [ym,yd,yh,yb,yp]

main :: IO ()
main = do
  print $ allValues findDaughters
