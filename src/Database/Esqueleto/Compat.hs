-- | This package aims to provide compatibility operators for @esqueleto@
-- and @persistent@ such that you can import "Database.Esqueleto.Compat"
-- without name conflicts.
--
-- This module re-exports "Database.Persist.Sql" and
-- "Database.Esqueleto.Experimental" together and hides the conflicting
-- terms in each module. Then we expose compatibility operators (like
-- '==.') that can work in either context, and names with suffixes to avoid
-- conflicts for other things (like 'updateE' instead of
-- 'Database.Esqueleto.Experimental.update').
module Database.Esqueleto.Compat
    ( -- * The compatibility operators
      module Database.Esqueleto.Compat.Operators
    , module Database.Esqueleto.Compat.Suffixed
    , -- * Re-exports from "Database.Esqueleto.Experimental" (or "Database.Esqueleto" in esqueleto >= 4)
        module Import.Database.Esqueleto
      -- * Re-exports from "Database.Persist.Sql"
    , module Database.Persist.Sql
    ) where

import Import.Database.Esqueleto hiding
    ( not_
    , update
    , selectSource
    , exists
    , delete
    , count
    , (-=.)
    , (*=.)
    , (+=.)
    , (&&.), (=.), (<.), (>=.), (||.), (/=.), (<=.), (>.), (!=.), (==.))
import Database.Esqueleto.Compat.Operators
import Database.Esqueleto.Compat.Suffixed
import Database.Persist.Sql hiding
    ( (-=.)
    , (*=.)
    , (+=.)
    , (/=.)
    , (<=.), (<.), (>=.), (=.), (||.), (>.), (/=.), (!=.), (==.))
