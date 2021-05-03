import Data.Word
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL


data Trade = Trade
  { timestamp :: !Word32
  , price     :: !Word32
  , qty       :: !Word16
  } deriving (Show)

getTrade :: Get Trade
getTrade = do
  timestamp <- getWord32le
  price     <- getWord32le
  quantity  <- getWord16le
  return $! Trade timestamp price quantity

getTrades :: Get [Trade]
getTrades = do
  empty <- isEmpty
  if empty
    then return []
    else do trade <- getTrade
            trades <- getTrades
            return (trade:trades)

lazyIOExample :: IO [Trade]
lazyIOExample = do
  input <- BL.readFile "trades.bin"
  return (runGet getTrades input)

main :: IO ()
main = do
  result <- lazyIOExample
  putStrLn "okay"


