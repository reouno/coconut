{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Entity.Internal.User where

import           Control.Lens
import           Data.Extensible
import           Data.Text

type User = Record '[ "name" >: Text, "email" >: Text, "password" >: Text]

type UserId = Int

type UserRecord = (UserId, User)
