import Data.Word
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)


data Trade = Trade
  { timestamp :: !Word32
  , price     :: !Word32
  } deriving (Show)

getTrade :: Get Trade
getTrade = do
  timestamp <- getWord32le
  price     <- getWord32le
  return $! Trade timestamp price

getTrades :: Get [Trade]
getTrades = do
  empty <- isEmpty
  if empty
    then return []
    else do trade <- getTrade
            trades <- getTrades
            return (trade:trades)

packStr'' :: String -> B.ByteString
packStr'' = encodeUtf8 . T.pack

lazyIOExample :: IO [Trade]
lazyIOExample = do
  input <- BL.readFile "trades.bin"
  return (runGet getTrades input)

main :: IO ()
main = do
  result <- lazyIOExample
  putStrLn "okay"


