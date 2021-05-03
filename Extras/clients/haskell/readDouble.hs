{-# LANGUAGE ForeignFunctionInterface #-}

-- https://wiki.haskell.org/Examples/Read_Double

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Internal       as B
import qualified Data.ByteString.Lazy.Char8 as L
import Foreign
import Foreign.C.Types
import System.IO.Unsafe

------------------------------------------------------------------------

--
-- read a Double from a lazy ByteString
-- 'read' should be a pure operation, right??
--
readDouble :: ByteString -> Double
readDouble ls = unsafePerformIO $ B.useAsCString s $ \cstr ->
    realToFrac `fmap` c_strtod cstr nullPtr
  where
    s = B.concat . L.toChunks $ ls

foreign import ccall unsafe "static stdlib.h strtod" c_strtod
    :: Ptr CChar -> Ptr (Ptr CChar) -> IO CDouble

------------------------------------------------------------------------
--
-- show a Double into a lazy bytestring
--

showDouble :: Double -> ByteString
showDouble d = L.fromChunks . return . unsafePerformIO . B.createAndTrim lim $  \p ->
    B.useAsCString fmt $ \cfmt -> do
        n <- c_printf_double (castPtr p) (fromIntegral lim) cfmt (realToFrac d)
        return (min lim (fromIntegral n)) -- snprintf might truncate
  where
    lim = 100 -- n.b.
    fmt = B.pack "%f"

foreign import ccall unsafe "static stdio.h snprintf" 
    c_printf_double :: Ptr CChar -> CSize -> Ptr CChar -> CDouble -> IO CInt

------------------------------------------------------------------------

--
-- And sum lazy bytestring doubles read from stdin
-- Using packed writes to output as well.
--
main = do L.interact $ showDouble . sum . map readDouble . L.lines
          putStrLn ""
