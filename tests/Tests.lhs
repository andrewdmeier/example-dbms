
Simple code to check that the client and server can talk to each
other. This will develop into the acceptance tests and the command
line client.

> import Database.ExampleDBMS.Network.ClientApi
> import Control.Monad (when)

> main :: IO ()
> main = do
>   putStrLn "connecting"
>   h <- connect "127.0.0.1" 4321
>   sth <- prepareStatement h "select * from t"
>   sty <- statementType sth
>   executeStatement sth
>   case sty of
>     Query cols -> do
>       putStrLn $ show cols
>       let lp = do
>                vs <- fetch sth
>                putStrLn $ show vs
>                when (not $ null vs) lp
>       lp
>     _ -> return ()
>   closeStatement sth
>   putStrLn "disconnecting"
>   disconnect h
