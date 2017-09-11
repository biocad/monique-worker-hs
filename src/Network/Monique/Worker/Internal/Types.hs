module Network.Monique.Worker.Internal.Types
  ( Processing, WorkerResult (..), UType, UData, Version, TaskResult (..)
  , throwWorkerError
  ) where

import           Control.Monad.Except       (ExceptT, throwError)
import           Network.Monique.Core       (UserId)
import           Network.Monique.Core.Data  (TaskResult (..), UData, UType,
                                             Version)
import           Network.Monique.Core.Error (MoniqueError (..))


type WorkerName = String

data WorkerResult = WorkerResult { taskResult   :: TaskResult
                                 , userdataList :: [(UType, UData)]
                                 } deriving (Show)

type Processing a = UserId -> a -> ExceptT MoniqueError IO WorkerResult

throwWorkerError :: WorkerName -> String -> ExceptT MoniqueError IO WorkerResult
throwWorkerError workerName = throwError . WorkerError workerName
