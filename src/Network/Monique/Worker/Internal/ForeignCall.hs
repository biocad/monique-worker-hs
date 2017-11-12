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
import           System.ZMQ4                           (Socket, Sub (..),
                                                        receiveMulti, send,
                                                        subscribe, unsubscribe)



callForeignWorker :: (ToJSON a, FromJSON b) => WorkerName -> a -> WorkerInfo -> Stateful s b
callForeignWorker workerName' taskConfigJSON WorkerInfo{..} = do
        let taskSpec = pack $ wName workerName'
        let taskConfig = toJSON taskConfigJSON
        let WorkerConnections{..} = connections
        taskId <- generateId
        newTask' <- newTask taskId (Just curTaskId) curUserId taskSpec taskConfig

        liftIO $ subscribe fromQueue $ tagsToBS (tagTaskStatusSpec Completed taskSpec)
        liftIO $ subscribe fromQueue $ tagsToBS (tagTaskStatusSpec Failed taskSpec)
        liftIO $ send toQueue [] . toBS . toQMessage $ newTask'

        Task{..} <- waitForReply taskId fromQueue -- TODO: probably place with deadlock if no one task is returned

        liftIO $ unsubscribe fromQueue $ tagsToBS (tagTaskStatusSpec Completed taskSpec)
        liftIO $ unsubscribe fromQueue $ tagsToBS (tagTaskStatusSpec Failed taskSpec)

        let throwWError = throwError . WorkerError (wName workerName)

        case tStatus of
            Completed -> lift . exceptDecodeValue . content . fromJust $ tResult
            Failed    -> throwWError $ unpack taskSpec ++ " (foreign worker): " ++ unpack (fromJust tMessage)
            status    -> throwWError $ "unexpected status foreign task: " ++ show status
  where
    waitForReply :: TaskId -> Socket Sub -> Stateful s Task
    waitForReply taskId fromQueue' = do
        [_, bytestring]  <- liftIO $ receiveMulti fromQueue'
        QMessage{..} <- lift . exceptDecodeBS $ bytestring
        case cnt of
            (T task'@Task{..}) | tId == taskId -> pure task'
            _                  -> waitForReply taskId fromQueue'

