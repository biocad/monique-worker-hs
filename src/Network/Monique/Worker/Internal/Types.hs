module Network.Monique.Worker.Internal.Types
  ( Algo, Stateful
  , WorkerName , WorkerResult (..), throwWorkerError
  , UType, UData, TaskResult (..)
  ) where

import           Control.Monad.Except       (ExceptT, throwError)
import           Control.Monad.State        (StateT)
import           Network.Monique.Core       (UserId)
import           Network.Monique.Core.Data  (TaskResult (..), UData, UType)
import           Network.Monique.Core.Error (MoniqueError (..))



type WorkerName = String

type Stateful s a = StateT s (ExceptT MoniqueError IO) a

data WorkerResult = WorkerResult { taskResult   :: TaskResult
                                 , userdataList :: [(UType, UData)]
                                 } deriving (Show)

type Algo a s =  UserId                  -- ^ use UserId to save userdata
              -> WorkerName              -- ^ use WorkerName to throw error and write log
              -> a                       -- ^ config to run algorithm
              -> Stateful s WorkerResult -- ^ return result in StateT monad

throwWorkerError :: WorkerName -> String -> Stateful s WorkerResult
throwWorkerError workerName = throwError . WorkerError workerName

