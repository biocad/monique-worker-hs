{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Network.Monique.Worker.Internal.App
  ( runApp, moniqueHost
  ) where

import           Control.Monad.Except                  (runExceptT)
import           Control.Monad.State                   (evalStateT, liftIO)
import           Data.Aeson                            (FromJSON (..))
import           Data.Maybe                            (fromMaybe)
import           Network.Monique.Core                  (Host, Port, moniqueHost)
import           Network.Monique.Worker.Internal.Queue (WorkerConfig (..),
                                                        runWorker)
import           Network.Monique.Worker.Internal.Types (Algo, WorkerName)
import           Options.Generic
import           System.IO                             (BufferMode (..),
                                                        hSetBuffering, stdout)


data RunOptions w =
  RunOptions { host :: w  ::: Maybe Host <?> "Host to controller (default: monique.bi.biocad.ru)"
             , port :: w  ::: Port       <?> "Port to controller"
             , hostS :: w ::: Maybe Host <?> "Host to scheduler (default: monique.bi.biocad.ru)"
             , portS :: w ::: Port       <?> "Port to scheduler (usually 4050 for Local/Develop and 5050 for Production)"
             } deriving (Generic)


instance ParseRecord (RunOptions Wrapped)
deriving instance Show (RunOptions Unwrapped)

runApp
  :: FromJSON a
  => WorkerName -- ^ worker name
  -> s          -- ^ initial state
  -> Algo a s   -- ^ algorithm to execute
  -> IO ()
runApp name initialState algo = do
    hSetBuffering stdout LineBuffering
    RunOptions{..} <- unwrapRecord "Monique worker start"
    let workerConfig = WorkerConfig (fromMaybe moniqueHost host) port (fromMaybe moniqueHost hostS) portS
    _ <- liftIO . runExceptT . evalStateT (runWorker name algo workerConfig) $ initialState
    pure ()
