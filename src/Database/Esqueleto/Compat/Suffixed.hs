{-# LANGUAGE CPP #-}

-- | This module defines new names for terms in @esqueleto@ that have
-- common conflicts with other modules.
--
-- Functions that perform "actions" are generally suffixed with an @E@. So
-- @update@ becomes @updateE@.
--
-- Functions that are about manipulating 'SqlExpr' are suffixed with an
-- @_@. So @isNothing@ becomes 'isNothing_'.
module Database.Esqueleto.Compat.Suffixed
    ( module Database.Esqueleto.Compat.Suffixed
    , isNothing_
    ) where

import Database.Esqueleto.Internal.Internal
import qualified Import.Database.Esqueleto as E
import Import.Database.Esqueleto
    ( SqlExpr
    , Entity, SqlQuery, PersistEntity, BackendCompatible, SqlBackend
    , PersistEntityBackend
    , PersistQueryWrite
    , PersistUniqueWrite
    )
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Conduit

updateE
    :: ( MonadIO m
        , PersistEntity record
        , BackendCompatible SqlBackend backend
        , BackendCompatible SqlBackend (PersistEntityBackend record)
        , PersistQueryWrite backend
        , PersistUniqueWrite backend
        )
    => (SqlExpr (Entity record) -> SqlQuery ()) -> ReaderT backend m ()
updateE = E.update

selectSourceE
    :: (SqlSelect a r
        , BackendCompatible SqlBackend backend
        , E.IsPersistBackend backend
        , E.PersistQueryRead backend
        , E.PersistUniqueRead backend
        , MonadResource m
        )
    => SqlQuery a
    -> ConduitT () r (ReaderT backend m) ()
selectSourceE =
    E.selectSource

exists_ :: SqlQuery () -> SqlExpr (Value Bool)
exists_ = E.exists

deleteE
    :: (MonadIO m
        , BackendCompatible SqlBackend backend, PersistQueryWrite backend, PersistUniqueWrite backend
        )
    => SqlQuery ()
    -> ReaderT backend m ()
deleteE = E.delete

count_ :: (Num a) => SqlExpr (Value typ) -> SqlExpr (Value a)
count_ = E.count

#if MIN_VERSION_esqueleto(3,5,10)

#else
isNothing_ :: (E.PersistField a) => SqlExpr (Value (Maybe a)) -> SqlExpr (Value Bool)
isNothing_ = E.isNothing

groupBy_ :: (ToSomeValues a) => a -> SqlQuery ()
groupBy_ = E.groupBy
#endif
