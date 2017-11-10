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

import           Control.Monad.Except
import           Control.Monad.State                   (evalStateT, liftIO)
import           Data.Aeson                            (FromJSON (..))
import           Data.Maybe                            (fromMaybe)
import           Network.Monique.Core                  (Host, Port, moniqueHost)
import           Network.Monique.Worker.Internal.Queue (WorkerConfig (..),
                                                        runWorker)
import           Network.Monique.Worker.Internal.Types (Algo)
import           Options.Generic
import           System.IO                             (BufferMode (..),
                                                        hSetBuffering, stdout)



data RunOptions w =
  RunOptions { name :: w  ::: String     <?> "Worker name for logging"
             , host :: w  ::: Maybe Host <?> "Host where controller lives (default: monique.bi.biocad.ru)"
             , port :: w  ::: Port       <?> "Port where controller lives"
             , hostS :: w ::: Maybe Host <?> "Host where scheduler lives (default: monique.bi.biocad.ru)"
             , portS :: w ::: Port       <?> "Port where scheduler lives (usually 4050 for Local/Develop and 5050 for Production)"
             } deriving (Generic)

instance ParseRecord (RunOptions Wrapped)
deriving instance Show (RunOptions Unwrapped)

runApp
  :: FromJSON a
  => s        -- ^ initial state
  -> Algo a s -- ^ algorithm to execute
  -> IO ()
runApp initialState algo = do
    hSetBuffering stdout LineBuffering
    RunOptions{..} <- unwrapRecord "Monique worker start"
    let workerConfig = WorkerConfig name (fromMaybe moniqueHost host) port (fromMaybe moniqueHost hostS) portS
    _ <- liftIO . runExceptT . evalStateT (runWorker algo workerConfig) $ initialState
    pure ()
