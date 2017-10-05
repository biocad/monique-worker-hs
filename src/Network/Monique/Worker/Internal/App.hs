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

import           Control.Monad.State                   (evalStateT)
import           Data.Aeson                            (FromJSON (..))
import           Data.Maybe                            (fromMaybe)
import           Network.Monique.Core                  (Host, Port, moniqueHost)
import           Network.Monique.Worker.Internal.Queue (WorkerConfig (..),
                                                        runWorker)
import           Network.Monique.Worker.Internal.Types (Processing)
import           Options.Generic



data RunOptions w = RunOptions { name :: w ::: String     <?> "Worker name for logging"
                               , host :: w ::: Maybe Host <?> "Host where controller live (default: monique.bi.biocad.ru)"
                               , port :: w ::: Port       <?> "Port where controller live"
                               } deriving (Generic)

instance ParseRecord (RunOptions Wrapped)
deriving instance Show (RunOptions Unwrapped)

runApp :: FromJSON a => s -> Processing a s -> IO ()
runApp initialState process = do
    RunOptions{..} <- unwrapRecord "Monique worker start"
    let workerConfig = WorkerConfig name (fromMaybe moniqueHost host) port
    evalStateT (runWorker process workerConfig) initialState
