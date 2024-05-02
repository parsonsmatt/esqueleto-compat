{-# LANGUAGE CPP #-}

-- | This module carries the imports for "Database.Esqueleto.Experimental"
-- in a manner that avoids the warnings and deprecations present with
-- 0.4.0.0.
module Import.Database.Esqueleto
    ( module X
    , SqlExpr_
    , ValueContext
    ) where

#if MIN_VERSION_esqueleto(4,0,0)
import Database.Esqueleto as X
import Database.Esqueleto.Internal.Internal (SqlExpr_, ValueContext)
#else
import Database.Esqueleto.Experimental as X
#endif

#if MIN_VERSION_esqueleto(4,0,0)

#else
-- | @esqueleto@ version 4 modified the 'SqlExpr' type to carry a context
-- type variable.
--
-- @
-- data SqlExpr_ context val
--
-- data ValueContext
-- data AggregateContext
--
-- type SqlExpr = SqlExpr_ ValueContext
-- @
--
-- We provide this type alias as a means of giving backwards compatibility
-- in the library - it should still build and test fine with @esqueleto
-- < 4@ while also supporting the additional polymorphism in @esqueleto >=
-- 4@.
type SqlExpr_ ignore = SqlExpr

-- | A dummy type to be thrown away by 'SqlExpr_'
data ValueContext
#endif
