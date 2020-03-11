{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.PureUserRepository where

import qualified Data.Map  as Map
import qualified Data.Text as T

type Username = T.Text

type Password = T.Text

type Website = T.Text

data User =
  User
    { user :: Username
    , pass :: Password
    , site :: Website
    }

-- could be a postgres connection, a file, anything.
type UserDB = Map.Map Username User

-- create a "database" from a list of users
createUserDB :: [User] -> UserDB
createUserDB users = Map.fromList [(k, v) | v@(User k _ _) <- users]

-- our test database
userDB :: UserDB
userDB =
  createUserDB
    [User "Neo" "matrix01" "neo.com", User "Trinity" "matrix02" "trinity.com"]
