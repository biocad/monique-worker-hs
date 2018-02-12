{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Network.Monique.Worker.Internal.Types
  ( Algo, Stateful
  , WorkerResult (..)
  , WorkerName (..)
  , WorkerConnections (..), WorkerInfo (..)
  , WorkerConfig (..)
  , throwWorkerError, throwWorkerErrorI
  , UType, UData, TaskResult (..), UserId, TaskMessage
  ) where

import           Control.Exception.Safe     (MonadThrow)
import           Control.Monad.Catch        (throwM)
import           Control.Monad.Except       (ExceptT, MonadError (..),
                                             throwError)
import           Control.Monad.State        (StateT (..))
import           Data.Text                  (Text)
import           Network.Monique.Core       (Host, Port, UserId)
import           Network.Monique.Core.Data  (TaskId, TaskMessage,
                                             TaskResult (..), UData, UType)
import           Network.Monique.Core.Error (MoniqueError (..))
import           System.ZMQ4                (Pull (..), Push (..), Socket,
                                             Sub (..))


newtype WorkerName = WorkerName { wName :: String }

type Stateful s = StateT s (ExceptT MoniqueError IO)

instance {-# OVERLAPPING #-} Monad (Stateful s) where
  return = pure
  -- TODO: empty wName is a crutch
  fail = throwError . WorkerError ""
  m >>= k = StateT $ \ s -> do
    ~(a, s') <- runStateT m s
    runStateT (k a) s'

instance {-# OVERLAPPING #-} MonadThrow (Stateful s) where
  throwM e = throwError . WorkerError "" $ show e

data WorkerResult = WorkerResult { taskResult   :: TaskResult
                                 , userdataList :: [(UType, UData)]
                                 } deriving (Show)

data WorkerInfo = WorkerInfo { curUserId   :: UserId            -- ^ use UserId to save userdata
                             , curTaskId   :: TaskId            -- ^ use TaskId to call foreign workers
                             , workerName  :: WorkerName        -- ^ use WorkerName to throw error and write log
                             , connections :: WorkerConnections -- ^ use WorkerConnections to conect to queue
                             , configT     :: Text              -- ^ use Text with config to load custom fields
                             }

data WorkerConfig =
     WorkerConfig { controllerH     :: Host
                  , fromControllerP :: Port
                  , queueH          :: Host
                  , fromQueueP      :: Port
                  , logfile         :: FilePath
                  , configText      :: Text
                  }

data WorkerConnections = WorkerConnections { toController   :: Socket Push
                                           , fromController :: Socket Pull
                                           , toQueue        :: Socket Push
                                           , fromQueue      :: Socket Sub
                                           }

type Algo a s
  =  WorkerInfo              -- ^ use it to generate errors and call foreign workers
  -> a                       -- ^ config to run algorithm
  -> Stateful s WorkerResult -- ^ return result in StateT monad

throwWorkerError :: WorkerName -> String -> Stateful s WorkerResult
throwWorkerError WorkerName{..} = throwError . WorkerError wName

throwWorkerErrorI :: WorkerInfo -> String -> Stateful s WorkerResult
throwWorkerErrorI WorkerInfo{..} = throwWorkerError workerName
