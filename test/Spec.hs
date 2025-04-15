{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# options_ghc -Wall #-}
module Main where

import Database.Esqueleto.Compat
import Database.Persist.TH
import Test.Hspec

mkPersist sqlSettings [persistLowerCase|

    User
        name    String
        age     Int

    Post
        author  UserId
        title   String

    Comment
        author  UserId
        post    PostId
        body    String
    |]

main :: IO ()
main = hspec spec

typechecks :: a -> IO ()
typechecks _ = pure ()

userTable :: SqlExpr (Entity User)
userTable = undefined

postTable :: SqlExpr (Entity Post)
postTable = undefined

commentTable :: SqlExpr (Entity Comment)
commentTable = undefined

spec :: Spec
spec = do
    sqlComparisonSpec
    sqlAssignmentSpec

sqlComparisonSpec :: Spec
sqlComparisonSpec = describe "SqlComparison" $ do
    describe "Persistent" $ do
        it "works with EntityField" $ do
            typechecks @(SqlPersistT IO _) $ do
                selectList [UserName ==. "hello"] []

        it "works with overloaded label" $ do
            typechecks @(SqlPersistT IO [Entity User]) $ do
                selectList [#name ==. "hello"] []

        it "works with polymorphic literal" $ do
            typechecks @(SqlPersistT IO _) $ do
                selectList [UserAge ==. 1] []

        it "works with literal and symbol" $ do
            typechecks @(SqlPersistT IO [Entity User]) $ do
                selectList [#age ==. 1] []

    describe "Esqueleto" $ do
        describe "comparing two tables" $ do
            it "works when typed in esqueleto context" $ do
                typechecks $ do
                    userTable ^. UserName ==. postTable ^. PostTitle
            it "works when using symbol to field" $ do
                typechecks $ do
                    userTable ^. #name ==. postTable ^. #title

            it "works when using overloaded record dot" $ do
                typechecks $ do
                    userTable.name ==. postTable.title

        describe "with val" $ do
            it "type inference works" $ do
                typechecks $ do
                    userTable ^. UserName ==. val "hello"
            it "even with polymorphic val" $ do
                typechecks $ do
                    userTable ^. UserAge ==. val 1
            it "with symbol to field" $ do
                typechecks $ do
                    userTable ^. #age ==. val 1
            it "with overloaded record dot" $ do
                typechecks $ do
                    userTable.age ==. val 1

        describe "with from" $ do
            it "infers with explicit table annotation" $ do
                typechecks $ do
                    u <- from $ table @User
                    where_ $ u.name ==. val "hello"
                    pure u

            it "infers with explicit field use" $ do
                typechecks $ do
                    u <- from table
                    where_ $ u ^. UserName ==. val "hello"
                    pure u

            it "infers with symbol to field" $ do
                typechecks $ do
                    u <- from $ table @User
                    where_ $ u ^. #name ==. val "hello"
                    pure u

            it "joins aren't painful" $ do
                typechecks $ do
                    u :& p :& c <-
                        from $
                            table @User
                                `innerJoin` table @Post
                                    `on` do
                                        \(u :& p) ->
                                            u.id ==. p.author
                                `innerJoin` table @Comment
                                    `on` do
                                        \(_ :& p :& c) ->
                                            p.id ==. c.post
                    where_ $ u.name ==. val "hello"
                    where_ $ p.title ==. val "asdf"
                    where_ $ c.body ==. val "no"
                    pure (u :& c)

sqlAssignmentSpec :: Spec
sqlAssignmentSpec = describe "SqlAssignment" $ do
    describe "Esqueleto" $ do
        it "works with explicit field" $ do
            typechecks @(SqlPersistT IO _) $ do
                updateE $ \user -> do
                    set user [UserAge =. val 10]
        it "works with symbol, as long as user is inferred" $ do
            typechecks @(SqlPersistT IO _) $ do
                updateE $ \user -> do
                    set user [#age =. val 10]
                    where_ $ user ^. UserName ==. val "hello"

    describe "Persistent" $ do
        pure ()
