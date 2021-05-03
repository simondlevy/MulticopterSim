import Data.Bits
import Network.Socket
import Network.Socket.ByteString
import Network.BSD
import Data.List
import Data.ByteString.Internal

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy.Char8 as L
import Foreign
import Foreign.C.Types
import System.IO.Unsafe

-- https://wiki.haskell.org/Examples/Read_Double --------------------------------------------

--
-- read a Double from a lazy ByteString
-- 'read' should be a pure operation, right??
--
readDouble :: L.ByteString -> Double
readDouble ls = unsafePerformIO $ B.useAsCString s $ \cstr ->
    realToFrac `fmap` c_strtod cstr nullPtr
  where
    s = B.concat . L.toChunks $ ls

foreign import ccall unsafe "static stdlib.h strtod" c_strtod
    :: Ptr CChar -> Ptr (Ptr CChar) -> IO CDouble


-- show a Double into a lazy bytestring
--

showDouble :: Double -> L.ByteString
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

-- // http://book.realworldhaskell.org/read/sockets-and-syslog.html -------------------------
type HandlerFunc = SockAddr -> Data.ByteString.Internal.ByteString -> IO ()

main :: IO ()
-- main = putStrLn "Hello, World!"
main = serveLog "5001" plainHandler

serveLog :: String              -- ^ Port number or name
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveLog port handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.  
       addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Bind it to the address we're listening to
       -- bindSocket sock (addrAddress serveraddr)
       bind sock (addrAddress serveraddr)

       -- Loop forever processing incoming data.  Ctrl-C to abort.
       procMessages sock
    where procMessages sock =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 -- (msg, _, addr) <- recvFrom sock 1024
                 (msg, addr) <- Network.Socket.ByteString.recvFrom sock 104
                 -- Handle it
                 handlerfunc addr msg
                 -- And process more messages
                 procMessages sock

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = 
    do
        -- putStrLn $ "From " ++ show addr ++ ": " 
        print (msg)
