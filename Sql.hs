{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Sql where

import Data.List (intercalate)

import DataModel

data Select = Select [SelectExpr] TableExpr (Maybe Expr)
                     -- , grouping :: GroupBy
                     -- , ordering :: OrderBy
                     -- , limit :: Limit
            deriving (Eq, Show)

data SelectExpr = SelectExpr Expr
                | AliasedExpr SelectExpr String
                deriving (Eq, Show)

data TableExpr = NoTable
               | TableRefExpr TableRef
               | InnerJoin TableExpr TableRef (Maybe Expr)
               | LeftJoin TableExpr TableRef (Maybe Expr)
               deriving (Eq, Show)

data TableRef = JustTable Table
              | TableSubQuery Select
              | AliasedTableRef TableRef String
              deriving (Eq, Show)

data Expr = OrExpr Expr Expr
          | XorExpr Expr Expr
          | AndExpr Expr Expr
          | NotExpr Expr
          -- | IsExpr { primary :: BoolPrimary, isNot :: Bool, isVal :: TrueFalseUnknown }
          | IsNull Expr
          | NullSafeEq Expr Expr
          | CompEq Expr Expr
          | CompGE Expr Expr
          | CompGT Expr Expr
          | CompLE Expr Expr
          | CompLT Expr Expr
          | CompNE Expr Expr
          | PredInSubQuery Expr Select --{ subquery :: Select, isNot :: Bool }
          | PredInList Expr [Expr] -- { exprs :: [Expr], isNot :: Bool }
          | PredBetween Expr Expr Expr
          | PredLike Expr Expr
          | PredRegEx Expr Expr
          | BitExprOr Expr Expr
          | BitExprAnd Expr Expr
          | BitExprLeftShift Expr Expr
          | BitExprRightShift Expr Expr
          | BitExprAdd Expr Expr
          | BitExprSub Expr Expr
          | BitExprMul Expr Expr
          | BitExprDiv Expr Expr
          | BitExprIntDiv Expr Expr
          | BitExprMod Expr Expr
          | BitExprXor Expr Expr
             -- | BitExprAddIntv BitExpr IntvExpr
             -- | BitExprSubIntv BitExpr IntvExpr
          | LiteralExpr Literal
          | FieldExpr Field TableRef
          | FuncExpr FunctionCall
          | NegExpr Expr
          | BitInvExpr Expr
          | TupleExpr [Expr]
          | SubqueryExpr Select
          | ExistsExpr Select
          -- | CaseExpr Case
          deriving (Eq, Show)

data Literal = StringLit String
             | NumberLit String
             deriving (Eq, Show)

data FunctionCall = FunctionCall String [Expr] deriving (Eq, Show)

class Compilable a where
  compile :: a -> String

instance Compilable Literal where
  compile (StringLit s) = "'" ++ s ++ "'"
  compile (NumberLit s) = s

instance Compilable FunctionCall where
  compile (FunctionCall name args) = name ++ "(" ++
                                     (intercalate ", " $ map compile args) ++
                                     ")"
instance Compilable Expr where
  compile (CompEq left right) = (compile left) ++ " = " ++ (compile right)
  compile (LiteralExpr lit) = compile lit
  compile (FieldExpr f (AliasedTableRef _ alias)) = alias ++ "." ++ (column f)
  compile (FieldExpr f (JustTable t)) = (tableName t) ++ "." ++ (column f)


instance Compilable SelectExpr where
  compile (SelectExpr expr) = compile expr
  compile (AliasedExpr expr alias) = (compile expr) ++ " AS " ++ alias

instance Compilable TableExpr where
  compile (TableRefExpr t) = compile t
  compile (InnerJoin left right condition) =
    (compile left) ++ " JOIN " ++ (compile right) ++ case condition of
      Nothing -> ""
      Just expr -> " ON " ++ (compile expr)

instance Compilable TableRef where
  compile (JustTable t) = tableName t
  compile (TableSubQuery s) = compile s
  compile (AliasedTableRef tr alias) = (compile tr) ++ " AS " ++ alias

instance Compilable Select where
  compile (Select exprs tables filter) =
    "SELECT " ++ (intercalate ", " $ map compile exprs) ++ " FROM " ++
    (compile tables) ++ (comp_where filter)
    where comp_where Nothing = ""
          comp_where (Just expr) = "WHERE " ++ compile expr

data Query a = Query Select a deriving (Eq, Show)

instance Monad Query where
  return a = Query (Select [] NoTable Nothing) a
  (>>=) (Query (Select exprs tableExpr whereExpr) a) f = (Query newSelect b)
    where newSelect = Select (exprs ++ moreExprs) newTableExpr whereExpr
          newTableExpr = case otherTableExpr of
            NoTable -> tableExpr
            TableRefExpr t -> InnerJoin tableExpr t Nothing
            InnerJoin NoTable t on -> InnerJoin tableExpr t on

          (Query (Select moreExprs otherTableExpr otherWhereExpr) b) = f a

class Selectable a where
  mkSelectExpr :: a -> SelectExpr

instance Selectable String where
  mkSelectExpr = SelectExpr . LiteralExpr . StringLit

select :: Selectable a => a -> Query SelectExpr
select a = Query (Select [expr] NoTable Nothing) expr
  where expr = mkSelectExpr a

class Fromable a where
  makeTableRef :: a -> TableRef

instance Fromable Table where
  makeTableRef = JustTable

instance Fromable TableRef where
  makeTableRef = id

from :: Fromable a => a -> Query TableRef
from a = Query (Select [] (TableRefExpr tableRef) Nothing) tableRef
  where tableRef = makeTableRef a

join :: Fromable a => a -> (TableRef -> Expr) -> Query TableRef
join from on = Query (Select [] joinRef Nothing) tableRef
  where tableRef = makeTableRef from
        joinRef = InnerJoin NoTable tableRef $ Just $ on tableRef

instance Compilable (Query a) where
  compile (Query select _) = compile select

class TableLike a where
  getField :: a -> String -> Expr

instance TableLike TableRef where
  getField tableRef@(JustTable t) fieldName =
    FieldExpr (getFieldFromTable t fieldName) tableRef
  getField tableRef@(AliasedTableRef (JustTable t) _) fieldName =
    FieldExpr (getFieldFromTable t fieldName) tableRef

alias :: Fromable a => a -> String -> TableRef
alias entity alias = alias' (makeTableRef entity)
  where alias' (AliasedTableRef tr _) = AliasedTableRef tr alias
        alias' tr = AliasedTableRef tr alias

query dm = do
  co <- from $ (alias (getTable dm "CollectionObject") "co")
  agent <- join (alias (getTable dm "Agent") "a") (
    \agent -> CompEq (getField agent "agentId") (getField co "collectionObjectId"))
  select "foo"


