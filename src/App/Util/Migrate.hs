{-# LANGUAGE ScopedTypeVariables #-}

module App.Util.Migrate where

import           Database.Persist.Sql   ( ConnectionPool )

import           App.PostgresRepository

main :: IO ()
main = runMigration

runMigration :: IO ()
runMigration = withPool $ \(pool :: ConnectionPool) -> doMigration pool
