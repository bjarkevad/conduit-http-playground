{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Conduit
import Control.Monad (void)
import Network.Socket
       (Family(AF_INET), SocketType(Datagram, Stream), SockAddr(SockAddrInet), Socket, accept, bind,
        defaultProtocol, listen, socket, close, tupleToHostAddress)
import Network.Socket.ByteString (recv, sendTo)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (intercalate)
import Data.Monoid ((<>))

main :: IO ()
main = runConduitRes $ bracketP bind' close
  (\s -> acceptRecvC s .| recvC .| printSink)

recvC :: Conduit (Socket, SockAddr) (ResourceT IO) (Socket, SockAddr, ByteString)
recvC = awaitForever (\(s, a) -> repeatMC $ lift (recv s 4096 >>= \b -> return (s, a, b)))

acceptRecvC :: Socket -> Producer (ResourceT IO) (Socket, SockAddr)
acceptRecvC s = bracketP (accept s) (close . fst) yield

printSink :: Show a => Consumer (Socket, SockAddr, a) (ResourceT IO) ()
printSink = mapM_C $ \(s, a, b) -> liftIO . void $ print b >> sendTo s res a

res :: ByteString
res = intercalate "\n" ["HTTP/1.1 301 Moved Permanently", "Server: Conduit HTTP Server"] <> "\n"

bind' :: IO Socket
bind' = do
  s <- socket AF_INET Stream defaultProtocol
  let p = 9002
  let h = tupleToHostAddress (127, 0, 0, 1)
  bind s (SockAddrInet p h)
  listen s 5
  return s
