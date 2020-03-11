module App.PostgresRepository where

import           Control.Monad.IO.Class            ( liftIO )
import           Control.Monad.Logger              ( runStderrLoggingT )
import           Data.Yaml.Config                  ( loadYamlSettings, useEnv )
import           Database.Persist.Postgresql
import           Database.Persist.Sql

import           InterfaceAdapter.Repository.Model ( migrateAll )

dbConf :: IO PostgresConf
dbConf = loadYamlSettings ["config/database-setting.yml"] [] useEnv

withPool :: (ConnectionPool -> IO a) -> IO a
withPool app = do
  conf <- dbConf
  runStderrLoggingT $
    withPostgresqlPool (pgConnStr conf) (pgPoolSize conf) $ \pool ->
      liftIO $ app pool

doMigration :: ConnectionPool -> IO ()
doMigration = runSqlPersistMPool $ runMigration migrateAll
