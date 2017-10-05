module Network.Monique.Worker.Internal.Types
  ( Processing, WorkerResult (..), UType, UData, TaskResult (..), WorkerName
  , throwWorkerError
  ) where

import           Control.Monad.Except       (Except, throwError)
import           Control.Monad.State        (StateT)
import           Network.Monique.Core       (UserId)
import           Network.Monique.Core.Data  (TaskResult (..), UData, UType)
import           Network.Monique.Core.Error (MoniqueError (..))

type WorkerName = String

data WorkerResult = WorkerResult { taskResult   :: TaskResult
                                 , userdataList :: [(UType, UData)]
                                 } deriving (Show)

type Processing a s = UserId -> WorkerName -> a -> StateT s IO (Except MoniqueError WorkerResult)

throwWorkerError :: WorkerName -> String -> StateT s IO (Except MoniqueError WorkerResult)
throwWorkerError workerName = pure . throwError . WorkerError workerName

