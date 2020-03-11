{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module InterfaceAdapter.Repository.Adapter.User where

import           Control.Lens
import           Data.Extensible
import qualified Data.Text                         as T
import           Data.Time                         ( UTCTime )
import           Database.Persist.Sql              ( Unique )
import qualified Database.Persist.Sql              as DB
import           PersistentUtil                    ( sqlKey2Int )

import           Entity.Entity
import qualified InterfaceAdapter.Repository.Model as Model

fromEntity :: UTCTime -> User -> Model.User
fromEntity = undefined

toEntity :: Model.User -> User
toEntity (Model.User name email password _) =
  #name @= name <: #email @= email <: #password @= password <: emptyRecord

fromEntityName2UniqueKey :: T.Text -> Unique Model.User
fromEntityName2UniqueKey = Model.UniqueName

toEntityId :: Model.Key Model.User -> UserId
toEntityId = sqlKey2Int

toEntityRecord :: DB.Entity Model.User -> UserRecord
toEntityRecord user =
  (toEntityId . DB.entityKey $ user, toEntity . DB.entityVal $ user)
