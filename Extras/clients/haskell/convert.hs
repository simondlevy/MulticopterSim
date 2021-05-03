import Data.Word
import Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Char8 as C
import Data.Char (ord)


-- https://stackoverflow.com/questions/3232074/what-is-the-best-way-to-convert-string-to-bytestring
packStr, packStr', packStr'' :: String -> B.ByteString
packStr   = B.pack . map (fromIntegral . ord)
packStr'  = C.pack
packStr'' = encodeUtf8 . T.pack

data Trade = Trade
  { timestamp :: !Word32
  , price     :: !Word32
  } deriving (Show)

getTrade :: Get Trade
getTrade = do
  timestamp <- getWord32le
  price     <- getWord32le
  return $! Trade timestamp price

main :: IO ()
main = do
  input <- BL.readFile "trades.bin"
  let input2 = packStr "hello"
  return (runGet getTrade input)
  print "hello"


