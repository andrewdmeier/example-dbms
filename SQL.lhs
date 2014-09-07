
Contains the simple sql syntax, parser and pretty printer

> {-# LANGUAGE DeriveDataTypeable #-}

> module SQL where

> data QueryExpr = Select [(ValueExpr, Maybe String)] String
>                  deriving (Eq,Show,Read,Data,Typeable)

> data ValueExpr = NumLit Int
>                | BinOp ValueExpr String ValueExpr
>                  deriving (Eq,Show,Read,Data,Typeable)

> data Statement = QueryStatement QueryExpr
>                | InsertStatement String [String] [[ValueExpr]]
>                | DeleteStatement String (Maybe ValueExpr)
>                | UpdateStatement String
>                                  [(String,ValueExpr)]
>                                  (Maybe ValueExpr)
>                  deriving (Eq,Show,Read,Data,Typeable)

> parseValueExpr :: String -> Either String ValueExpr
> parseValueExpr = undefined

> parseQueryExpr :: String -> Either String QueryExpr
> parseQueryExpr = undefined

> parseStatement :: String -> Either String Statement
> parseStatement = undefined
