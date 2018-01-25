{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Network.Monique.Worker.Internal.App
  ( runApp
  ) where

import           Control.Monad.Except                  (runExceptT)
import           Control.Monad.State                   (evalStateT, liftIO)
import           Data.Aeson                            (FromJSON (..))
import           Data.Aeson.Picker                     ((|--))
import           Data.Text.IO                          as TIO (readFile)
import           Network.Monique.Worker.Internal.Queue (WorkerConfig (..),
                                                        runWorker)
import           Network.Monique.Worker.Internal.Types (Algo, WorkerName (..))
import           Options
import           System.IO                             (BufferMode (..),
                                                        hSetBuffering, stdout)
import           System.Log.Formatter
import           System.Log.Handler                    (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Logger

newtype AppOptions = AppOptions { optConfigFile :: FilePath }

instance Options AppOptions where
  defineOptions = pure AppOptions
    <*> simpleOption "config-file" "config.json"
        "Path to file with configurations."

runApp
  :: FromJSON a
  => WorkerName -- ^ worker name
  -> s          -- ^ initial state
  -> Algo a s   -- ^ algorithm to execute
  -> IO ()
runApp name@WorkerName{..} initialState algo = do
    hSetBuffering stdout LineBuffering
  
    runCommand $ \AppOptions{..} _ -> do
        wc@WorkerConfig{} <- loadConfig optConfigFile

        -- setup logging
        h <- fileHandler (logfile wc) DEBUG >>= \lh -> return $
          setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
        updateGlobalLogger wName (addHandler h)
        updateGlobalLogger wName (setLevel DEBUG)

        _ <- liftIO . runExceptT . evalStateT (runWorker name algo wc) $ initialState
        pure ()

-- | load from config file as in conventions here: https://api.biocad.ru/Infrastructure/%D0%9A%D0%BE%D0%BD%D0%B2%D0%B5%D0%BD%D1%86%D0%B8%D0%B8/config.json
loadConfig :: FilePath -> IO WorkerConfig
loadConfig configPath = do
  configText <- TIO.readFile configPath
  let controllerH' = configText |-- ["deploy", "monique", "host"]
  let controllerP' = configText |-- ["deploy", "monique", "port"]
  let queueH'      = configText |-- ["deploy", "monique", "host-scheduler"]
  let queueP'      = configText |-- ["deploy", "monique", "port-scheduler"]
  let logfile'     = configText |-- ["deploy", "monique", "logfile"]
  pure $ WorkerConfig controllerH' controllerP' queueH' queueP' logfile' configText
