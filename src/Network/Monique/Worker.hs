module Network.Monique.Worker
  ( UserId, TaskMessage, Algo, WorkerResult (..), WorkerName
  , UType, UData, TaskResult (..)
  , runApp, moniqueHost, throwWorkerError
  ) where

import           Network.Monique.Core                  (UserId)
import           Network.Monique.Core.Data             (TaskMessage)
import           Network.Monique.Worker.Internal.App   (moniqueHost, runApp)
import           Network.Monique.Worker.Internal.Types (Algo,
                                                        TaskResult (..), UData,
                                                        UType, WorkerName,
                                                        WorkerResult (..),
                                                        throwWorkerError)
