
This is the network server executable

> import Network
> import Network.Socket (withSocketsDo)
> --import Network.BSD
> import Control.Exception (finally)
> import System.IO
> --import Data.Word
> --import Control.Applicative
> import Control.Monad
> import Control.Concurrent
> import qualified Data.ByteString as B
> import qualified Data.ByteString.Lazy as BL
> import Data.Binary
> import Protocol as P
> import System.Environment
> import ServerApi as S

> main :: IO ()
> main = do
>     x <- getArgs
>     case x of
>       ["create_cluster",f] -> createCluster f
>       [f] -> runServer f
>       _ -> error $ "unrecognised args: " ++ show x

> runServer :: FilePath -> IO ()
> runServer clusterPath = do
>     sh <- startServer clusterPath
>     withSocketsDo $ listenAt 4321 $ \h ->
>         void $ forkIO $ connection sh h `finally` hClose h


> connection :: ServerHandle -> Handle -> IO ()
> connection sh h = waitForConnection
>   where
>     waitForConnection = do
>       m <- readMessage h
>       case m of
>         Connect -> do
>             putStrLn "connected"
>             writeMessage h Connected
>             sess <- createSession sh
>             serverBaseState sess
>         _ -> writeMessage h $ Error $ "waitForConnection, expected Connect, got " ++ show m
>     serverBaseState sess = do
>       m <- readMessage h
>       case m of
>         Disconnect -> do
>             putStrLn "disconnected"
>             closeSession sess
>             writeMessage h Disconnected
>             hClose h
>         PrepareStatement sql -> do
>             sth <- prepareStatement sess sql
>             writeMessage h StatementPrepared
>             statementPrepared sess sth
>         _ -> writeMessage h $ Error $ "serverBaseState, expected Disconnect, PrepareStatement, got " ++ show m
>     statementPrepared sess sth = do
>       m <- readMessage h
>       case m of
>         Disconnect -> do
>             putStrLn "disconnected"
>             closeSession sess
>             writeMessage h Disconnected
>             hClose h
>         CloseStatement -> do
>             closeStatement sth
>             writeMessage h StatementClosed
>             serverBaseState sess
>         GetStatementType -> do
>             st <- statementType sth
>             let t = case st of
>                         S.Command -> P.Command
>                         S.Query cs -> P.Query cs
>             writeMessage h $ StatementTypeIs t
>             statementPrepared sess sth
>         ExecuteStatement -> do
>             executeStatement sth
>             writeMessage h StatementExecuted
>             statementExecuted sess sth
>         _ -> writeMessage h $ Error $ "statementPrepared, expected Disconnect, CloseStatement, GetStatementType, ExecuteStatement, got " ++ show m

>     statementExecuted sess sth = do
>       m <- readMessage h
>       case m of
>         Disconnect -> do
>             putStrLn "disconnected"
>             closeSession sess
>             writeMessage h Disconnected
>             hClose h
>         CloseStatement -> do
>             closeStatement sth
>             writeMessage h StatementClosed
>             serverBaseState sess
>         GetStatementType -> do
>             st <- statementType sth
>             let t = case st of
>                         S.Command -> P.Command
>                         S.Query cs -> P.Query cs
>             writeMessage h $ StatementTypeIs t
>             statementExecuted sess sth
>         Fetch -> do
>             x <- fetch sth
>             writeMessage h $ Data x
>             statementExecuted sess sth
>         _ -> writeMessage h $ Error $ "statementExecuted, expected Disconnect, CloseStatement, GetStatementType, Fetch, got " ++ show m


> readMessage :: Handle -> IO Message
> readMessage h = do
>   b <- B.hGetSome h 16384 -- todo: bad number
>   return $ decode $ BL.fromStrict b

> writeMessage :: Handle -> Message -> IO ()
> writeMessage h m = do
>   B.hPut h $ BL.toStrict $ encode m
>   hFlush h
