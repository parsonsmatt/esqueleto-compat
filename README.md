# esqueleto-compat

This library aims to provide compatibility operators that can allow esqueleto and persistent to be imported together.

Operators like `==.` and `>=.` are defined as class members, and you can use them in the same module.
Functions like `update` that are shared in both libraries are given an `E` suffix for the `esqueleto` version.
And functions for operating on `SqlExpr` are given an `_` suffix when the name would otherwise be a conflict.

## Example:

```haskell
import Database.Esqueleto.Compat

foo :: MonadIO m => TableId -> SqlPersistT m ()
foo tableKey = do
    -- Esqueleto:
    updateE $ \table -> do
        set [table ^. TableField =. val "Hello"] table
        where_ $ table ^. TableId ==. val tableKey

    -- Persistent
    update tableKey [TableField =. "Goodbye"]

    -- Esqueleto:
    select $ do
        pure $ exists_ $ do
            t <- from $ table @Table
            where_ $ t ^. TableField ==. val "Hello"

    -- Persistent
    exists [TableField ==. val "Hello"]
```
