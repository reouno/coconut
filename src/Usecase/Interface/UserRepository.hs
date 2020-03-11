{-# LANGUAGE GADTs #-}

module Usecase.Interface.UserRepository where

-- TODO: 抽象化できたらこの依存は無くなるはずなので削除
import           Database.Persist.Sql

-- TODO: ユーザーを抽象化できたらこの依存は無くなるはずなので削除
import qualified InterfaceAdapter.Repository.Model        as Model

-- TODO: インターフェイスでこれに依存するのはおかしい。そもそもインターフェイスじゃなくて実装だな
import           InterfaceAdapter.Repository.Adapter.User ( fromEntityName2UniqueKey,
                                                            toEntityRecord )

import           Conduit                                  ( MonadUnliftIO )
import qualified Data.Text                                as T

import           Entity.Entity

getUser :: MonadUnliftIO m => T.Text -> ConnectionPool -> m (Maybe UserRecord)
getUser name pool =
  flip runSqlPool pool $
  fmap toEntityRecord <$> getBy (fromEntityName2UniqueKey name)
