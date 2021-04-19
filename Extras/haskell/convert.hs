-- https://stackoverflow.com/questions/12514013/lazily-read-and-manipulate-float-from-stdin-in-haskell
import qualified Data.ByteString.Lazy as B
import Data.Binary.IEEE754
import Data.Binary.Get

-- gives a list of doubles read from stdin
listOfFloat64le = do
  empty <- isEmpty
  if empty
     then return []
     else do v <- getFloat64le
             rest <- listOfFloat64le
             return (v : rest)


-- delay signal by one
delay us = 0 : us

-- feedback system, add delayed version of signal to signal
sys us = zipWith (+) us (delay us)

main = do
    input <- B.getContents
    let hs = sys $ runGet listOfFloat64le input
    print $ take 10 hs
