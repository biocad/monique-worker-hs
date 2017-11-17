{-# LANGUAGE RecordWildCards #-}

module Network.Monique.Worker.Internal.ForeignCall
  ( callForeignWorker
  ) where

import           Control.Monad.Except                  (throwError)
import           Control.Monad.State                   (lift, liftIO)
import           Data.Aeson                            (FromJSON (..),
                                                        ToJSON (..))
import           Data.Maybe                            (fromJust)
import           Data.Text                             (pack, unpack)
import           Network.Monique.Core                  (generateId)
import           Network.Monique.Core.Data             (Task (..), TaskId,
                                                        TaskResult (..),
                                                        TaskStatus (..),
                                                        newTask)
import           Network.Monique.Core.Error            (MoniqueError (..))
import           Network.Monique.Core.Json             (exceptDecodeBS,
                                                        exceptDecodeValue, toBS)
import           Network.Monique.Core.Queue            (QContent (..),
                                                        QMessage (..),
                                                        tagTaskStatusSpec,
                                                        tagsToBS, toQMessage)
import           Network.Monique.Worker.Internal.Types (Stateful,
                                                        WorkerConnections (..),
                                                        WorkerInfo (..),
                                                        WorkerName (..))
import           System.Log.Logger                     (errorM, infoM)
import           System.ZMQ4                           (Socket, Sub (..),
                                                        receiveMulti, send,
                                                        subscribe, unsubscribe)


callForeignWorker :: (ToJSON a, FromJSON b) => WorkerName -> a -> WorkerInfo -> Stateful s b
callForeignWorker workerName' taskConfigJSON WorkerInfo{..} = do

    let logFunc level msg = liftIO . level (wName workerName) $ wName workerName' ++ " (foreign worker): " ++ msg
    logFunc infoM "calling..."
    
    let taskSpec = pack $ wName workerName'
    let taskConfig = toJSON taskConfigJSON
    let WorkerConnections{..} = connections
    taskId <- generateId
    newTask' <- newTask taskId (Just curTaskId) curUserId taskSpec taskConfig

    liftIO $ subscribe fromQueue $ tagsToBS (tagTaskStatusSpec Completed taskSpec)
    liftIO $ subscribe fromQueue $ tagsToBS (tagTaskStatusSpec Failed taskSpec)
    liftIO $ send toQueue [] . toBS . toQMessage $ newTask'

    Task{..} <- waitForReply taskId fromQueue -- TODO: probably place with deadlock if no one task is returned
    logFunc infoM "get reply..."
    liftIO $ unsubscribe fromQueue $ tagsToBS (tagTaskStatusSpec Completed taskSpec)
    liftIO $ unsubscribe fromQueue $ tagsToBS (tagTaskStatusSpec Failed taskSpec)

    let throwWError = throwError . WorkerError (wName workerName)

    case tStatus of
        Completed -> do
          logFunc infoM "task completed"
          lift . exceptDecodeValue . content . fromJust $ tResult
        Failed    -> do
          logFunc errorM "task failed"
          throwWError $ unpack taskSpec ++ " (foreign worker): " ++ unpack (fromJust tMessage)
        status    -> do
          logFunc errorM "unexpected status foreign task"
          throwWError $ "unexpected status foreign task: " ++ show status
  where
    waitForReply :: TaskId -> Socket Sub -> Stateful s Task
    waitForReply taskId fromQueue' = do
        [_, bytestring]  <- liftIO $ receiveMulti fromQueue'
        QMessage{..} <- lift . exceptDecodeBS $ bytestring
        case cnt of
            (T task'@Task{..}) | tId == taskId -> pure task'
            _                  -> waitForReply taskId fromQueue'

