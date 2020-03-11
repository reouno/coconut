{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module App.BasicAuth
  ( app
  ) where

import           Conduit                          ( MonadUnliftIO )
import           Control.Concurrent
import           Control.Exception
import           Control.Lens
import           Data.Extensible
import qualified Data.Map                         as Map
import qualified Data.Text                        as T
import           Data.Text.Encoding               ( decodeUtf8 )
import           Database.Persist.Sql
import           Network.HTTP.Client              ( defaultManagerSettings, newManager )
import           Servant
import           Servant.Client

import qualified App.PureUserRepository           as Pure

--import           Usecase.Interface.Repository ( MonadConnection (..) )
import           App.PostgresRepository           ( withPool )
import           Entity.Entity
import           Usecase.Interface.UserRepository

-- type BasicAuthApp = IO
-- instance MonadConnection BasicAuthApp where
--   withPool handler pool =
-- Our API will contain a single endpoint, returning the authenticated user's own website.
type API = BasicAuth "People's websites" User :> "mysite" :> Get '[ JSON] T.Text

api :: Proxy API
api = Proxy

-- The server implementation "gets" an argument of the authenticated user type used with `BasicAuth`,
-- `User` in our case.
-- `BasicAuth` adds an argument just like `Capture`, `QueryParam`, `ReqBody`, and friends.
server :: Server API
server usr = return "Authorized!!"

-- checkBasicAuth :: Pure.UserDB -> IO (BasicAuthCheck Pure.User)
-- checkBasicAuth db =
--   return $
--   BasicAuthCheck $ \basicAuthData ->
--     let username = decodeUtf8 (basicAuthUsername basicAuthData)
--         password = decodeUtf8 (basicAuthPassword basicAuthData)
--      in case Map.lookup username db of
--           Nothing -> return NoSuchUser
--           Just u ->
--             if Pure.pass u == password
--               then return (Authorized u)
--               else return BadPassword
checkBasicAuth' :: ConnectionPool -> BasicAuthCheck User
checkBasicAuth' pool =
  BasicAuthCheck $ \basicAuthData -> do
    let username = decodeUtf8 (basicAuthUsername basicAuthData)
        password = decodeUtf8 (basicAuthPassword basicAuthData)
    mayUser <- getUser username pool
    case mayUser of
      Nothing -> return NoSuchUser
      Just (userId, user) ->
        if user ^. #password == password
          then return (Authorized user)
          else return BadPassword

-- mkApp :: Pure.UserDB -> IO Application
-- mkApp db = do
--    ctx <- flip (:.) EmptyContext <$> checkBasicAuth db
--    return $ serveWithContext api ctx server
mkApp' :: ConnectionPool -> IO Application
mkApp' pool = do
  let ctx = checkBasicAuth' pool :. EmptyContext
  return $ serveWithContext api ctx server

app :: IO Application
app = withPool mkApp'
