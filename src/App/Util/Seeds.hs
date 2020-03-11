{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module App.Util.Seeds where

import           Conduit                           ( MonadUnliftIO )
import           Control.Lens
import           Control.Monad                     ( forM_ )
import           Control.Monad.IO.Class            ( liftIO )
import           Data.Extensible
import           Data.Text                         hiding ( map, zip )
import           Data.Time
import           Data.Time.Clock.POSIX             ( utcTimeToPOSIXSeconds )
import           Database.Persist.Sql

import           App.PostgresRepository            ( withPool )
import qualified InterfaceAdapter.Repository.Model as Model

main :: IO ()
main = plantSeeds

plantSeeds :: IO ()
plantSeeds =
  withPool $ \(pool :: ConnectionPool) -> do
    putStrLn "seeds were planted."
    insertUsers pool

insertUsers :: ConnectionPool -> IO ()
insertUsers pool =
  forM_ (zip timestamps users) $ \(tm, user) -> runSqlPool (insert_ user) pool
  where
    timestamps =
      [ read "1999-09-11 00:00:00" :: UTCTime
      , read "1812-09-11 00:00:00" :: UTCTime
      , read "1995-12-31 12:13:14" :: UTCTime
      ]

users :: [Model.User]
users = [user1, user2]

user1 :: Model.User
user1 =
  Model.User
    "neo"
    "neo@matrix.com"
    "neo01"
    (read "2019-11-11 11:11:11" :: UTCTime)

user2 :: Model.User
user2 =
  Model.User
    "trinity"
    "trinity@matrix.com"
    "trinity01"
    (read "2019-11-11 11:11:11" :: UTCTime)
