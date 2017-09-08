{-# LANGUAGE RecordWildCards #-}

module Network.Monique.Worker.Internal.Queue
  ( runWorker, WorkerConfig (..)
  ) where

import           Control.Monad                         (forever)
import           Control.Monad.Except
import           Control.Monad.IO.Class                (liftIO)
import           Data.Aeson                            (FromJSON (..))
import           Data.ByteString                       (ByteString)
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
                                                        QMessageable (..),
                                                        createAndConnect)
import           Network.Monique.Worker.Internal.Types (Processing,
                                                        WorkerResult (..))
import           System.ZMQ4                           (Pull (..), Push (..),
                                                        Socket, context,
                                                        receive, send)


data WorkerConfig = WorkerConfig { name            :: String
                                 , controllerH     :: Host
                                 , fromControllerP :: Port
                                 }

runWorker :: FromJSON a => Processing a -> WorkerConfig -> IO ()
runWorker processing WorkerConfig{..} = do
    (toController, fromController) <- connections
    forever $
        receive fromController >>=
        runExceptT . process toController >>=
        either print pure -- TODO: log but not print

     where
       (_, toControllerP) = twinPort fromControllerP
       connections :: IO (Socket Push, Socket Pull)
       connections = do
           context' <- context
           toController <- createAndConnect context' Push controllerH toControllerP
           fromController <- createAndConnect context' Pull controllerH fromControllerP
           return (toController, fromController)

       process :: Socket Push -> ByteString -> ExceptT MoniqueError IO ()
       process toController messageByteString = do
              QMessage{..} <- exceptDecodeBS messageByteString
              case cnt of
                (T task@Task{..}) -> do
                  let tryResult = do
                          taskConfig <- exceptDecodeValue tConfig
                          WorkerResult{..} <- processing tUser taskConfig

                          completedTask <- completeTask task taskResult
                          userdata <- mapM (uncurry $ newUserdata tUser Created) userdataList

                          _ <- liftIO . sequence $ (send toController [] . toBS . toQMessage) <$> userdata
                          _ <- liftIO . send toController [] . toBS . toQMessage $ completedTask
                          pure ()

                  _ <- tryResult `catchError` \err -> do
                        failedTask <- liftIO . failTask task . pack . show $ err

                        liftIO . send toController [] . toBS . toQMessage $ failedTask
                        throwError $ WorkerError "worker" (show err)
                  pure ()

                other -> throwError . ParseError . UnexpectedMsgType . show $ other
