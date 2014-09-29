
Scratch code developing the testing and stuff

> {-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
> module Database.ExampleDBMS.Values.TableValues where

> import Data.Char (isSpace)
> import Data.List.Split (splitOn)
> import Data.List (intercalate, transpose, genericTake, sort)
> import qualified Text.Tabular.AsciiArt as Tb
> import qualified Text.Tabular as Tb
> import qualified Database.SQLite.Simple as SQ
> import Data.String (fromString)
> import Data.Data (Data,Typeable)
> import Data.Maybe (fromMaybe)
> import Text.Read (readMaybe)
> import Control.Arrow (second)

> data TableValue = TableValue [String] -- column names
>                              [[Integer]] -- row values
>                 deriving Show

> data DatabaseValue = DatabaseValue [(String,TableValue)]
>                    deriving Show

> parseCSV :: String -> [[Integer]]
> parseCSV s = map parseLine $ filter ne $ lines s
>   where
>     ne = not . null . trim
>     trim = let f = dropWhile isSpace . reverse
>            in f . f
>     parseLine :: String -> [Integer]
>     parseLine l = let es = splitOn "," l
>                   in map (read . trim) es


parse csv
pretty to csv

> prettyCSV :: [[Integer]] -> String
> prettyCSV ns = intercalate "\n" $ map (intercalate "," . map show) ns

pretty to table layout

> prettyTable :: TableValue -> String
> prettyTable (TableValue h d) =
>   Tb.render id id show
>   $ Tb.Table (Tb.Group Tb.NoLine [Tb.Group Tb.NoLine $ replicate (length d) $ Tb.Header ""])
>           (Tb.Group Tb.NoLine [Tb.Group Tb.NoLine $ map Tb.Header h])
>     d



data gen

> data TableGen = TableGen [(String,ColumnGen)] RowSpec
> data RowSpec = FixedRows Integer
> data ColumnGen = ColumnRand Integer Integer
>                | ColumnKey

> generateTable :: TableGen -> TableValue
> generateTable (TableGen cols rs) =
>   let nrs = case rs of FixedRows i -> i
>   in TableValue (map fst cols) $ transpose $ map (genRow nrs . snd) cols
>   where
>     genRow :: Integer -> ColumnGen -> [Integer]
>     genRow n ColumnKey = genericTake n [0..]
>     genRow n (ColumnRand l h) = genericTake n $ map (+l) $ map (`mod` (h - l)) [0..]

insert to sqlite

sqlite query -> table value

todo 1: split these two functions
todo 2: get the proper column names from sqlite?

> runQuery :: String -> DatabaseValue -> IO TableValue
> runQuery sql (DatabaseValue tbs) = do
>     -- create the tables
>     let cts = map makeCreateTable tbs
>     --putStrLn $ intercalate "\n" cts
>     conn <- SQ.open ""
>     mapM_ (SQ.execute_ conn . fromString) cts
>     -- insert the data
>     mapM_ (insertData conn) tbs
>     -- run the query
>     (rs :: [[Integer]]) <- SQ.query_ conn (fromString sql)
>     -- convert to result type
>     -- putStrLn $ show rs
>     -- fake column names
>     let ts = map (map toInt) rs
>         ns = case ts of
>                 x:_ -> length x
>                 _ -> 0
>         nms = take ns $ map (:[]) ['a'..]
>     return $ TableValue nms ts
>   where
>     --toInt :: Integer -> Int
>     toInt = id -- fromIntegral
>     insertData conn (tb,TableValue _ vals) = do
>         SQ.execute_ conn $ fromString $
>             "insert into " ++ tb ++ " values "
>             ++ intercalate "," (map makeRow vals)
>     makeRow vals = "(" ++ intercalate "," (map show vals) ++ ")"
>     --showVal i = show i --(Value x) | Just i <- fromDynamic x = show i
>     --showVal x = error "SqlLite unsupported value " ++ show x
>     makeCreateTable :: (String,TableValue) -> String
>     makeCreateTable (n,TableValue cns vals) =
>         "create table "
>         ++ n ++ " (" ++ intercalate "," (fieldDefs cns vals) ++ ");"
>     -- if there are no values in the table, we can't tell what the
>     -- type is at the moment, just set it to int not null for not
>     fieldDefs cns [] = map (\n -> (n ++ " int not null")) cns
>     fieldDefs cns (vs:_) = zipWith (\n _ -> n ++ " " ++ "int not null") cns vs
>     --tn (IntValue {}) = "int"

> dbV :: DatabaseValue
> dbV = DatabaseValue
>       [("t", generateTable $ TableGen [("a",ColumnKey), ("b",ColumnRand 5 15)] $ FixedRows 15)
>       ,("u", generateTable $ TableGen [("a",ColumnKey), ("b",ColumnRand 5 15)] $ FixedRows 15)
>       ]


> test :: IO ()
> test = putStrLn $ prettyTable $ generateTable $ TableGen [("a",ColumnKey), ("b",ColumnRand 5 15)] $ FixedRows 15


table value equals. The columns must be in the same order. The column
names are ignored currently (this should be fixed). The row order is
ignored.

> tableEquals :: TableValue -> TableValue -> Bool
> tableEquals (TableValue _ as) (TableValue _ bs) =
>     sort as == sort bs

sql syntax + physops for select lists + simple scalar expr

> data QueryExpr =
>     Select
>       {qeSelectList :: [(ValueExpr,Maybe String)]
>       ,qeFrom :: String
>       ,qeWhere :: Maybe ValueExpr
>       }

> data ValueExpr =
>       NumLit String
>     | Iden String
>     | Star
>     --  | App String [ValueExpr]
>     | BinOp ValueExpr String ValueExpr
>     | Parens ValueExpr
>       deriving (Eq,Show,Read,Data,Typeable)

ops:

=, +

-------
physops syntax

> data PhysOp =
>       Relvar String
>     | PSelect [(ValueExpr, String)] PhysOp

TODO: for the initial version, can leave out the physops completely

--------

physops interpreter

> evalValueExpr :: [(String,Integer)] -> ValueExpr -> Integer
> evalValueExpr _env (NumLit n) =
>      fromMaybe (error $ "PhysOpInterpreter.lhs bad integer: " ++ n)
>      (readMaybe n :: Maybe Integer)

> evalValueExpr env (Iden i) =
>     fromMaybe (error $ "PhysOpInterpreter.lhs Identifier not recognised: " ++ i)
>     $ lookup i env
> evalValueExpr _env Star = error "PhysOpInterpreter.lhs star not supported"
> evalValueExpr env (Parens e) = evalValueExpr env e
> evalValueExpr env (BinOp e0 o e1) =
>     let v0 = evalValueExpr env e0
>         v1 = evalValueExpr env e1
>     in case o of
>           "+" -> v0 + v1
>           _ -> error $ "unrecognised bin op " ++ o


> evalPhysop :: PhysOp -> DatabaseValue -> TableValue
> evalPhysop po dbv = uncurry TableValue $ evalPhysop' po dbv

> evalPhysop' :: PhysOp -> DatabaseValue -> ([String],[[Integer]])

> evalPhysop' (Relvar r) (DatabaseValue db) = maybe (error $ "PhysOpInterpreter.lhs table not found " ++ r)
>                                (\(TableValue cs vs) -> (cs,vs))
>                                $ lookup r db

> evalPhysop' (PSelect es p) db =
>     (map snd es, evalRows valss)
>   where
>     (nms,valss) = evalPhysop' p db
>     evalField :: [Integer] -> (ValueExpr, String) -> [Integer]
>     evalField vals (Star,_) = vals
>     evalField vals (e,_n) = [evalValueExpr (zip nms vals) e]
>     evalRow :: [Integer] -> [Integer]
>     evalRow vals = concatMap (evalField vals) es
>     evalRows :: [[Integer]] -> [[Integer]]
>     evalRows = map evalRow

-------

physops -> sql

this is so the physops can be tested via a sql engine

> physOpToSql :: PhysOp -> QueryExpr
> physOpToSql (PSelect es (Relvar r)) = Select (map (second Just) es) r Nothing

----------

sql pp

> ppSql :: QueryExpr -> String
> ppSql (Select es r whr) =
>   "select " ++ intercalate "," (map ppsi es)
>   ++ " from " ++ r
>   ++ maybe "" (\x -> " where " ++ ppse x) whr
>   where
>     ppsi (x,n) = ppse x
>                  ++ maybe "" (\y -> " as " ++ y) n
>     ppse (NumLit s) = s
>     ppse (Iden s) = s
>     ppse Star = "*"
>     ppse (BinOp e0 o e1) = ppse e0 ++ " " ++ o ++ " " ++ ppse e1
>     ppse (Parens e) = "(" ++ ppse e ++ ")"

----------

physops tests

> data TestItem = Group String [TestItem]
>               | PhysopTest [(String,TableGen)] PhysOp
>               | SqlTest [(String,TableGen)] String

--------

sql -> physops

> sqlToPhysOp :: PhysOp -> QueryExpr
> sqlToPhysOp _ = undefined
> -- sqlToPhysOp (Select es f Nothing) = PSelect es (Relvar r)

---------

sql parser

> parseSQL :: String -> Either String QueryExpr
> parseSQL = undefined

------

correctness tests for sql

implement insert, update and delete + sanity tests

> data Statement = QueryStatement QueryExpr
>                | Insert String [[Int]]
>                | Delete String (Maybe ValueExpr)
>                | Update String [(String,ValueExpr)] (Maybe ValueExpr)
>                | CreateTable String [String]
>                | DropTable String

implement create + drop table

---- milestone

implement create and drop database
design network protocol
create network server + client lib + create cluster
sql parser
tests via client library
simple concurrency + tests

---- milestone
complete basic spec
sql parser ++ pp
sql syntax for extra operations (compare + csv ops?)
physical ops parsing + pp
add types: bool, 16,64int, decimal, ascii text
scalar fn, operators, casts
restrict
join

catalog
---- milestone

constraints:
primary key
unique
foreign key
check
assertion
(exclusion, fdk?)

nulls + not null constraint + fix ops

---- milesone

add some more real basics:
simple aggregates + simple group by
simple order by
combined select lists/aggs
aggregate contexts (having and order by)

defaults, sequences
views
schemas
truncate
alter table
domains

------ milestone

extensibility: tpyes, functions, etc, + sql functions, + sql structs?

----

coverage?, do sooner?

-----

lots more query features

-----

more range and more depth to dml and ddl

----

in memory only db without concurrency?
without constraints?

