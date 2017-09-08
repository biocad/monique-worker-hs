module Network.Monique.Worker.Internal.Types
  ( Processing, WorkerResult (..), UType, UData, Version, TaskResult (..)
  ) where

import           Control.Monad.Except       (ExceptT)
import           Network.Monique.Core       (UserId)
import           Network.Monique.Core.Data  (TaskResult (..), UData, UType,
                                             Version)
import           Network.Monique.Core.Error (MoniqueError)



data WorkerResult = WorkerResult { taskResult   :: TaskResult
                                 , userdataList :: [(UType, UData)]
                                 } deriving (Show)

type Processing a = UserId -> a -> ExceptT MoniqueError IO WorkerResult
