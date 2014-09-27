
simple network client and server code wrapper

> module Database.ExampleDBMS.Network.Network where

> import Network.Socket
> import Network.BSD
> import Control.Exception (finally)
> import System.IO
> import Data.Word
> import Control.Applicative

> connectTo :: String -> Int -> IO Handle
> connectTo host port_ = do
>       let port = toEnum port_
>       sock <- socket AF_INET Stream 0
>       setSocketOption sock ReuseAddr 1
>       setSocketOption sock NoDelay 1
>       setSocketOption sock KeepAlive 1
>       addrs <- hostAddresses <$> getHostByName host
>       case addrs of
>         [] -> error $ "no such host : " ++ host
>         addr:_ -> do
>           connect sock $ SockAddrInet port addr
>           handle <- socketToHandle sock ReadWriteMode
>           hSetBuffering handle $ BlockBuffering Nothing --  $ Just 65536
>           -- hSetBuffering handle $ NoBuffering -- Nothing --  $ Just 65536
>           return handle

> listenAt :: Int -> (Handle -> IO ()) -> IO ()
> listenAt port_ f = do
>       let port = toEnum port_
>       lsock <- socket AF_INET Stream 0
>       setSocketOption lsock ReuseAddr 1
>       setSocketOption lsock NoDelay 1
>       setSocketOption lsock KeepAlive 1
>       bindSocket lsock $ SockAddrInet port iNADDR_ANY
>       listen lsock sOMAXCONN
>       loop lsock `finally` sClose lsock
>    where
>       loop lsock = do
>         (sock,SockAddrInet _ _ip) <- accept lsock
>         handle <- socketToHandle sock ReadWriteMode
>         hSetBuffering handle $ BlockBuffering Nothing
>         f handle
>         loop lsock
