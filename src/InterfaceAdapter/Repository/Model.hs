{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module InterfaceAdapter.Repository.Model where

import           Data.Text
import           Data.Time            ( UTCTime )
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH
import           GHC.Generics         ( Generic )

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User join
    name Text
    email Text
    UniqueName name
    password Text
    createdAt UTCTime default=CURRENT_TIMESTAMP
    deriving Read Eq Ord Generic Show
|]
