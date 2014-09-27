
The network protocol messages definitions

> {-# LANGUAGE DeriveDataTypeable,TemplateHaskell #-}
> module Database.ExampleDBMS.Network.Protocol where

> import Data.Binary
> --import GHC.Generics (Generic)
> import Data.Data
> import Data.DeriveTH
> --import Data.Derive.Binary

> data StatementType = Command
>                    | Query [String] -- the names of the result columns
>       deriving (Eq,Show,Data,Typeable)

> $( derive makeBinary ''StatementType )

> data Message
>     = Connect
>     | Connected
>     | Error String
>     | Disconnect
>     | Disconnected
>     | PrepareStatement String
>     | StatementPrepared
>     | GetStatementType
>     | StatementTypeIs StatementType
>     | ExecuteStatement
>     | StatementExecuted
>     | Fetch
>     | Data [[Integer]]
>     | CloseStatement
>     | StatementClosed
>       deriving (Eq,Show,Data,Typeable)

> $( derive makeBinary ''Message )
