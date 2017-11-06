{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Monique.Worker.Internal.Queue
  ( runWorker, WorkerConfig (..)
  ) where

import           Control.Monad                         (forever)
import           Control.Monad.Catch                   (SomeException, catch)
import           Control.Monad.Except                  (catchError)
import           Control.Monad.State                   (lift, liftIO)
import           Data.Aeson                            (FromJSON (..))
import           Data.Text                             (pack)
import           Network.Monique.Core                  (Host, Port, twinPort)
import           Network.Monique.Core.Data             (Property (..),
                                                        Task (..), completeTask,
                                                        failTask, newUserdata)
import           Network.Monique.Core.Error            (MoniqueError (..))
import           Network.Monique.Core.Json             (exceptDecodeBS,
                                                        exceptDecodeValue, toBS)
import           Network.Monique.Core.Queue            (QContent (..),
                                                        QMessage (..),
                                                        createAndConnect,
                                                        toQMessage)
import           Network.Monique.Worker.Internal.Types (Algo, Stateful,
                                                        WorkerConnections (..),
                                                        WorkerInfo (..),
                                                        WorkerName,
                                                        WorkerResult (..))
import           System.ZMQ4                           (Pull (..), Push (..),
                                                        Sub (..), context,
                                                        receive, send)



data WorkerConfig =
     WorkerConfig { name            :: WorkerName
                  , controllerH     :: Host
                  , fromControllerP :: Port
                  , queueH          :: Host
                  , fromQueueP      :: Port
                  }

runWorker
    :: FromJSON a
    => Algo a s
    -> WorkerConfig
    -> Stateful s ()
runWorker algo WorkerConfig{..} = do
    workerConnections@WorkerConnections{..} <- liftIO connections
    forever $ do
        msg                  <- liftIO $ receive fromController
        QMessage{cnt = cnt'} <- lift   $ exceptDecodeBS msg
        case cnt' of
          T task@Task{} -> runAlgo algo workerConnections task `catchError` moniqueErrorHandler
          other         -> liftIO . print $ "Error Network.Monique.Worker.Internal.Queue: " ++ show other

  where
    (_, toControllerP) = twinPort fromControllerP
    (_, toQueueP)      = twinPort fromQueueP

    connections :: IO WorkerConnections
    connections = do
        context' <- context
        toController   <- createAndConnect context' Push controllerH toControllerP
        fromController <- createAndConnect context' Pull controllerH fromControllerP
        toQueue        <- createAndConnect context' Push queueH toQueueP
        fromQueue      <- createAndConnect context' Sub  queueH fromQueueP
        pure $ WorkerConnections toController fromController toQueue fromQueue

    moniqueErrorHandler :: MoniqueError -> Stateful s ()
    moniqueErrorHandler err = liftIO (print err) >> pure ()

    runAlgo
        :: FromJSON a
        => Algo a s
        -> WorkerConnections
        -> Task
        -> Stateful s ()
    runAlgo algo' workerConnections@WorkerConnections{..} task@Task{..} =
      executeAlgo algo' `catchError` workerExceptionHandler
                        `catch` (\(x :: SomeException) -> workerExceptionHandler x)
      where
        executeAlgo :: FromJSON a => Algo a s -> Stateful s ()
        executeAlgo algo'' = do
            taskConfig       <- lift $ exceptDecodeValue tConfig
            let workerInfo = WorkerInfo tUser tId name workerConnections
            WorkerResult{..} <- algo'' workerInfo taskConfig

            completedTask    <- completeTask task taskResult
            userdatas        <- mapM (uncurry $ newUserdata tUser Created) userdataList

            _ <- liftIO . mapM (send toController [] . toBS . toQMessage) $ userdatas
            _ <- liftIO .       send toController [] . toBS . toQMessage  $ completedTask
            pure ()

        workerExceptionHandler :: Show a => a -> Stateful s ()
        workerExceptionHandler err = do
            failedTask <- failTask task . pack . show $ err
            liftIO . send toController [] . toBS . toQMessage $ failedTask

