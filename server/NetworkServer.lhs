
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
> import Database.ExampleDBMS.Network.Protocol as P
> import System.Environment
> import Database.ExampleDBMS.Network.Server

> import Database.ExampleDBMS.Server.ServerApi as S

> main :: IO ()
> main = do
>     x <- getArgs
>     case x of
>       ["create_cluster",f] -> createCluster f
>       [f] -> runServer f
>       _ -> error $ "unrecognised args: " ++ show x

