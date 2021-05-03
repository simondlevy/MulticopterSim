import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Char (ord)

packStr, packStr', packStr'' :: String -> B.ByteString
packStr   = B.pack . map (fromIntegral . ord)
packStr'  = C.pack
packStr'' = encodeUtf8 . T.pack

main :: IO ()
main = do
  print (packStr "\NULhellö♥")


