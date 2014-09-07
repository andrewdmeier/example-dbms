
This is the server side api for working with the dbms.

> module ServerApi where

creates the disk structure for a set of database managed by a single
dbms server

> createCluster :: FilePath -> IO ()
> createCluster fp = return ()



> data ServerHandle = ServerHandle Int

this starts a server on the cluster path given. There can only be one
server active on a cluster at one time.

> startServer :: FilePath -> IO ServerHandle
> startServer fp = return $ ServerHandle 1

Stops the running server. This is the fast stop version: immediately
cancel all open transactions, and leave any temporary junk on the
filesystem without cleaning it. Other more gentle methods of stopping
the server can be implemented.

> stopServer :: ServerHandle -> IO ()
> stopServer _ = return ()

a session encapsulates a set of state and the ability to run up to one
statement (this is modelled after postgres and may be changed). The
network server will map each client connection to a session.

> data SessionHandle = SessionHandle Int

> createSession :: ServerHandle -> IO SessionHandle
> createSession sh = return $ SessionHandle 1

close session cleans up all the resources associated with the
session. This can be called when there is an open statement/
transaction and this will be cancelled and cleaned up correctly.

> closeSession :: SessionHandle -> IO ()
> closeSession _ = return ()

> data StatementHandle = StatementHandle Int

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

> prepareStatement :: SessionHandle -> String -> IO StatementHandle
> prepareStatement sh sql = return $ StatementHandle 1

> data StatementType = Command
>                    | Query [String] -- the names of the result columns

not sure which of these need to be in IO

this can be used to tell if the prepared statement is a command or
query and thus what the next steps are for the client.

> statementType :: StatementHandle -> IO StatementType
> statementType _ = return $ Query ["a"]

This starts a query running, or completely executes a command

> executeStatement :: StatementHandle -> IO ()
> executeStatement sh = return ()

> fetch :: StatementHandle -> IO [[Integer]]
> fetch sh = return [[1]]

> closeStatement :: StatementHandle -> IO ()
> closeStatement _ = return ()

