module Database.Esqueleto.Compat.Operators where

import Database.Esqueleto.Experimental (SqlExpr, Value)
import qualified Database.Esqueleto.Experimental as Esqueleto
import qualified Database.Esqueleto.Internal.Internal as Esqueleto
import Database.Persist.Sql (Entity, EntityField, PersistEntity, PersistField, Filter)
import qualified Database.Persist.Sql as Persist
import GHC.TypeLits
import Data.Kind

-- | A class for assigning a value in SQL, shared among the @persistent@ and
-- @esqueleto@ libraries.
class SqlAssignment a b c where
  (=.) :: a -> b -> c
  (-=.) :: a -> b -> c
  (+=.) :: a -> b -> c
  (*=.) :: a -> b -> c

infixr 3 =.

instance
  ( PersistField typ
  , field ~ EntityField rec typ'
  , typ ~ typ'
  ) =>
  SqlAssignment field typ (Persist.Update rec)
  where
  (=.) = (Persist.=.)
  (-=.) = (Persist.-=.)
  (+=.) = (Persist.+=.)
  (*=.) = (Persist.*=.)

instance
  (PersistEntity rec, PersistField typ, field ~ EntityField rec typ) =>
  SqlAssignment field (SqlExpr (Value typ)) (SqlExpr (Entity rec) -> SqlExpr Esqueleto.Update)
  where
  (=.) = (Esqueleto.=.)
  (-=.) = (Esqueleto.-=.)
  (+=.) = (Esqueleto.+=.)
  (*=.) = (Esqueleto.*=.)

-- Esqueleto: (||.) :: SqlExpr (Value Bool) -> SqlExpr (Value Bool) -> SqlExpr (Value Bool)
-- Persistent: (||.) :: [Filter v] -> [Filter v] -> [Filter v]

-- | A class for abstracting over 'Bool'-like operations.
--
-- Irritatingly, we can't have 'not_' in here, because @persistent@ actually
-- doesn't have such a function! That's why the 'SqlBooleanNot' class exists.
class SqlBoolean a where
  true_ :: a
  false_ :: a
  (||.) :: a -> a -> a
  (&&.) :: a -> a -> a

infixr 3 &&.

infixr 2 ||.

class SqlBoolean a => SqlBooleanNot a where
  not_ :: a -> a

-- | 'SqlExpr' can be compared as 'SqlBoolean' values, provided that they
-- contain a @'Value' 'Bool'@.
--
-- The implementation uses the @(a ~ Bool)@ equality constraint so that
-- polymorphic definitions don't get too confused.
instance (a ~ Bool) => SqlBoolean (SqlExpr (Value a)) where
  true_ = Esqueleto.val True
  false_ = Esqueleto.val False
  (||.) = (Esqueleto.||.)
  (&&.) = (Esqueleto.&&.)

instance (a ~ Bool) => SqlBooleanNot (SqlExpr (Value a)) where
  not_ = Esqueleto.not_

-- | This is a bit of a weird definition.
--
-- Turns out, 'Persistent.||.' is very rarely used in the codebase - we actually
-- have more uses of 'Persist.FilterOr'! And there *isn't* a '&&.' in
-- @persistent@ at all.
instance SqlBoolean [Persist.Filter k] where
  true_ = []
  false_ = [Persist.FilterOr []]
  (||.) = (Persist.||.)
  (&&.) = (<>)

-- | A 'TypeError' instance is provided so that folks don't get too confused,
-- though I doubt they'll run into this.
instance
  ( TypeError
      ( 'Text "`persistent` does not have a `not_` operator for filters. Instead, use the "
          ':$$: 'Text "inverse operator, like `<.` instead of `>=.`."
      )
  ) =>
  SqlBooleanNot [Persist.Filter t]
  where
  not_ = error "unreachable"

-- | A class for comparing for equality in @persistent@ and @esqueleto@. The
-- first two type parameters are the inputs to the binary operator, and the
-- final one is the result type.
class SqlComparison a b c | c a -> b, c b -> a, a b -> c where
  (==.) :: a -> b -> c
  (!=.) :: a -> b -> c
  (>.) :: a -> b -> c
  (>=.) :: a -> b -> c
  (<.) :: a -> b -> c
  (<=.) :: a -> b -> c

infix 4 ==., !=., /=., >=., >., <=., <.

type family NotSqlExprEq rec typ' typ :: Constraint where
  NotSqlExprEq rec typ' (SqlExpr (Value _)) =
    TypeError (NotSqlExprEqMessage rec typ')
  NotSqlExprEq _ _ _ =
    ()

type NotSqlExprEqMessage rec typ =
  'Text "You used a bare `"
    ':<>: 'ShowType (EntityField rec typ)
    ':<>: 'Text "` field."
    ':$$: 'Text "If you're writing a Persistent expression, you don't need to use `val`."
    ':$$: 'Text "If you're writing an esqueleto expression, you need to project from a "
    ':$$: 'Text "table variable, like: e ^. FooName"

instance
  (lhs ~ EntityField rec typ, PersistField typ, typ ~ typ', NotSqlExprEq rec typ typ', rec ~ rec') =>
  SqlComparison (EntityField rec typ) typ' (Filter rec')
  where
  (==.) = (Persist.==.)
  (!=.) = (Persist.!=.)
  (>.) = (Persist.>.)
  (>=.) = (Persist.>=.)
  (<.) = (Persist.<.)
  (<=.) = (Persist.<=.)

instance
  (PersistField a, a ~ b, lhs ~ SqlExpr (Value a), c ~ Bool) =>
  SqlComparison (SqlExpr (Value a)) (SqlExpr (Value b)) (SqlExpr (Value c))
  where
  (==.) = (Esqueleto.==.)
  (!=.) = (Esqueleto.!=.)
  (>.) = (Esqueleto.>.)
  (>=.) = (Esqueleto.>=.)
  (<.) = (Esqueleto.<.)
  (<=.) = (Esqueleto.<=.)

-- | An alias for '!=.', in keeping with the convention of having Haskell-ish
-- operators.
(/=.) :: SqlComparison a b c => a -> b -> c
(/=.) = (!=.)
