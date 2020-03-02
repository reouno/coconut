{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module BasicAuth
  ( runApp
  , userDB
  ) where

import           Control.Concurrent
import           Control.Exception
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import           Data.Text.Encoding       ( decodeUtf8 )
import           Network.HTTP.Client      ( defaultManagerSettings, newManager )
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client

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

-- Our API will contain a single endpoint, returning the authenticated user's own website.
type API
   = BasicAuth "People's websites" User :> "mysite" :> Get '[ JSON] Website

api :: Proxy API
api = Proxy

-- The server implementation "gets" an argument of the authenticated user type used with `BasicAuth`,
-- `User` in our case.
-- `BasicAuth` adds an argument just like `Capture`, `QueryParam`, `ReqBody`, and friends.
server :: Server API
server usr = return (site usr)

checkBasicAuth :: UserDB -> BasicAuthCheck User
checkBasicAuth db =
  BasicAuthCheck $ \basicAuthData ->
    let username = decodeUtf8 (basicAuthUsername basicAuthData)
        password = decodeUtf8 (basicAuthPassword basicAuthData)
     in case Map.lookup username db of
          Nothing -> return NoSuchUser
          Just u ->
            if pass u == password
              then return (Authorized u)
              else return BadPassword

runApp :: UserDB -> IO ()
runApp db = run 8080 (serveWithContext api ctx server)
  where
    ctx = checkBasicAuth db :. EmptyContext
