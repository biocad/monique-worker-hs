{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Monique.Worker.Internal.Queue
  ( runWorker, WorkerConfig (..)
  ) where

import           Control.Monad                         (forever)
import           Control.Monad.Catch                   (SomeException, catch)
import           Control.Monad.Except                  (runExcept,
                                                        runExceptT, throwError)
import           Control.Monad.State                   (StateT, lift, liftIO)
import           Data.Aeson                            (FromJSON (..))
import           Data.Text                             (pack)
import           Network.Monique.Core                  (Host, Port, twinPort)
import           Network.Monique.Core.Data             (Property (..),
                                                        Task (..), completeTask,
                                                        failTask, newUserdata)
import           Network.Monique.Core.Error            (MoniqueError (..),
                                                        ParseError (..))
import           Network.Monique.Core.Json             (exceptDecodeBS,
                                                        exceptDecodeValue, toBS)
import           Network.Monique.Core.Queue            (QContent (..),
                                                        QMessage (..),
                                                        createAndConnect,
                                                        toQMessage)
import           Network.Monique.Worker.Internal.Types (Processing, WorkerName,
                                                        WorkerResult (..))
import           System.ZMQ4                           (Pull (..), Push (..),
                                                        Socket, context,
                                                        receive, send)

data WorkerConfig = WorkerConfig { name            :: WorkerName
                                 , controllerH     :: Host
                                 , fromControllerP :: Port
                                 }

runWorker :: FromJSON a => Processing a s -> WorkerConfig -> StateT s IO ()
runWorker processing WorkerConfig{..} = do
    (toController, fromController) <- lift connections
    forever $ do
        messageByteString <- lift (receive fromController)

        let parseQMessage = do
                QMessage{..} <- exceptDecodeBS messageByteString
                case cnt of
                  T t@Task{..} -> exceptDecodeValue tConfig >>= \config -> pure (config, t)
                  other        -> throwError . ParseError . UnexpectedMsgType . show $ other

        parsedEither <- lift $ runExceptT parseQMessage
        case parsedEither of
            Right (config, task@Task{..}) -> do

                eitherResult <- (runExcept <$> processing tUser name config) `catch` catchSomeException

                let goodWay WorkerResult{..} = do
                      completedTask <- completeTask task taskResult
                      userdata <- mapM (uncurry $ newUserdata tUser Created) userdataList

                      _ <- liftIO . sequence $ (send toController [] . toBS . toQMessage) <$> userdata
                      _ <- liftIO . send toController [] . toBS . toQMessage $ completedTask
                      pure ()
                let badWay err = do
                      failedTask <- failTask task . pack . show $ err
                      _ <- liftIO . send toController [] . toBS . toQMessage $ failedTask
                      pure ()

                either badWay goodWay eitherResult

            Left err -> lift $ print err >> pure ()


     where
       (_, toControllerP) = twinPort fromControllerP
       connections :: IO (Socket Push, Socket Pull)
       connections = do
           context' <- context
           toController <- createAndConnect context' Push controllerH toControllerP
           fromController <- createAndConnect context' Pull controllerH fromControllerP
           return (toController, fromController)

       catchSomeException :: SomeException -> StateT s IO (Either MoniqueError WorkerResult)
       catchSomeException err = liftIO $ pure $ Left $ WorkerError name (show err)
