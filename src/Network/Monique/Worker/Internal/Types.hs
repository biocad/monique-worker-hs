{-# LANGUAGE RecordWildCards #-}

module Network.Monique.Worker.Internal.Types
  ( Algo, Stateful
  , WorkerResult (..)
  , WorkerName , WorkerConnections (..), WorkerInfo (..)
  , throwWorkerError, throwWorkerErrorI
  , UType, UData, TaskResult (..), UserId, TaskMessage
  ) where

import           Control.Monad.Except       (ExceptT, throwError)
import           Control.Monad.State        (StateT)
import           Network.Monique.Core       (UserId)
import           Network.Monique.Core.Data  (TaskId, TaskResult (..), UData, TaskMessage,
                                             UType)
import           Network.Monique.Core.Error (MoniqueError (..))
import           System.ZMQ4                (Push (..), Socket, Sub (..), Pull (..))


type WorkerName = String

type Stateful s a = StateT s (ExceptT MoniqueError IO) a

data WorkerResult = WorkerResult { taskResult   :: TaskResult
                                 , userdataList :: [(UType, UData)]
                                 } deriving (Show)

data WorkerInfo = WorkerInfo { curUserId        :: UserId      -- ^ use UserId to save userdata
                             , curTaskId        :: TaskId      -- ^ use TaskId to call foreign workers
                             , workerName :: WorkerName  -- ^ use WorkerName to throw error and write log
                             , connections :: WorkerConnections
                             }

data WorkerConnections = WorkerConnections { toController :: Socket Push
                                           , fromController :: Socket Pull
                                           , toQueue :: Socket Push
                                           , fromQueue :: Socket Sub
                                           }

type Algo a s
  =  WorkerInfo
  -> a                       -- ^ config to run algorithm
  -> Stateful s WorkerResult -- ^ return result in StateT monad

throwWorkerError :: WorkerName -> String -> Stateful s WorkerResult
throwWorkerError wn = throwError . WorkerError wn

throwWorkerErrorI :: WorkerInfo -> String -> Stateful s WorkerResult
throwWorkerErrorI WorkerInfo {..} = throwError . WorkerError workerName

