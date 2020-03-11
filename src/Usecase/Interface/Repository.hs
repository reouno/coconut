module Usecase.Interface.Repository where

import           Database.Persist.Sql

class Monad m =>
      MonadConnection m
  where
  withPool :: (pool -> m a) -> pool -> m b
  -- doMigration :: pool -> IO ()
