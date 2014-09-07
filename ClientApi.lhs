
Wrapper for the network access. This is used in client applications.

> module ClientApi where

> import Network
> import Protocol
> import System.IO
> import qualified Data.ByteString as B
> import qualified Data.ByteString.Lazy as BL
> import Data.Binary

> data ConnectionHandle = ConnectionHandle Handle

> connect :: String -> Int -> IO ConnectionHandle
> connect host port = do
>   h <- connectTo host port
>   m <- exchange h Connect
>   case m of
>     Connected -> return $ ConnectionHandle h
>     _ -> errorMessage "Connected" m

> exchange :: Handle -> Message -> IO Message
> exchange h mo = do
>   writeMessage h mo
>   x <- readMessage h
>   return x

> errorMessage :: String -> Message -> a
> errorMessage _ (Error s) = error s
> errorMessage m x = error $ show i ++ " unexpected server response, waiting for: " ++ m ++ ", got: " ++ show x


> readMessage :: Handle -> IO Message
> readMessage h = do
>   b <- B.hGetSome h 16384 -- todo: bad number
>   return $ decode $ BL.fromStrict b

> writeMessage :: Handle -> Message -> IO ()
> writeMessage h m = do
>   B.hPut h $ BL.toStrict $ encode m
>   hFlush h

the database is set using a SQL-like command.

close session cleans up all the resources associated with the
session. This can be called when there is an open statement/
transaction and this will be cancelled and cleaned up correctly.

> disconnect :: ConnectionHandle -> IO ()
> disconnect (ConnectionHandle h) = do
>   m <- exchange h Disconnect
>   case m of
>     Disconnected -> hClose h
>     _ -> hClose h >> errorMessage "disconnect" m

> data StatementHandle = StatementHandle ConnectionHandle

prepares a statement ready to run. This will check the statement for
validity, but doesn't reserve any runtime resources.

running commands:
prepareStatement
executeStatement
closeStatement

running queries
prepareStatement
executeStatement
fetch (repeat until empty)
closeStatement

running unknown statement type:
prepareStatement
st <- statementType
executeStatement
if st is query
    fetch (repeat until empty)
closeStatement

> prepareStatement :: ConnectionHandle -> String -> IO StatementHandle
> prepareStatement ch@(ConnectionHandle h) sql = do
>   m <- exchange h $ PrepareStatement sql
>   case m of
>     StatementPrepared -> return $ StatementHandle ch
>     _ -> errorMessage "prepare" m

> data StatementType = Command
>                    | Query [String] -- the names of the result columns

not sure which of these need to be in IO

this can be used to tell if the prepared statement is a command or
query and thus what the next steps are for the client.

> statementType :: StatementHandle -> IO ClientApi.StatementType
> statementType (StatementHandle (ConnectionHandle h)) = do
>   m <- exchange h $ GetStatementType
>   case m of
>     StatementTypeIs s -> return $ case s of
>       Protocol.Command -> ClientApi.Command
>       Protocol.Query cs -> ClientApi.Query cs
>     _ -> errorMessage "getstatementtype" m

This starts a query running, or completely executes a command

> executeStatement :: StatementHandle -> IO ()
> executeStatement (StatementHandle (ConnectionHandle h)) = do
>   m <- exchange h ExecuteStatement
>   case m of
>     StatementExecuted -> return ()
>     _ -> errorMessage "executeStatement" m

> fetch :: StatementHandle -> IO [[Integer]]
> fetch  (StatementHandle (ConnectionHandle h)) = do
>   m <- exchange h Fetch
>   case m of
>     Data d -> return d
>     _ -> errorMessage "fetch" m

> closeStatement :: StatementHandle -> IO ()
> closeStatement  (StatementHandle (ConnectionHandle h)) = do
>   m <- exchange h CloseStatement
>   case m of
>     StatementClosed -> return ()
>     _ -> errorMessage "closeStatement" m

