{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON (..), ToJSON (..))
import           GHC.Generics           (Generic)
import qualified Network.Monique.Worker as W (Algo, TaskResult (..),
                                              WorkerName (..),
                                              WorkerResult (..),
                                              callForeignWorker, runApp)

main :: IO ()
main = W.runApp exampleABName () exampleABProcess

newtype ExampleABConfig = ExampleABConfig { configAB :: String }
  deriving (Generic)
instance FromJSON ExampleABConfig
instance ToJSON ExampleABConfig

newtype ExampleABResult = ExampleABResult { resultAB :: Float }
  deriving (Show, Generic)
instance ToJSON ExampleABResult
instance FromJSON ExampleABResult

version :: Int
version = 1

exampleABName :: W.WorkerName
exampleABName = W.WorkerName "exampleAB"

exampleABProcess :: W.Algo ExampleABConfig ()
exampleABProcess workerInfo (ExampleABConfig exampleABConfig') = do
    liftIO $ print exampleABConfig'

    let configA = ExampleAConfig exampleABConfig'
    ea@ExampleAResult{..} <- W.callForeignWorker exampleAName configA workerInfo
    liftIO . print $ "After worker exampleA:" ++ show ea

    let configB = ExampleBConfig $ fromIntegral resultA
    eb@ExampleBResult{..} <- W.callForeignWorker exampleBName configB workerInfo
    liftIO . print $ "After worker exampleB:" ++ show eb

    let response = ExampleABResult (sin resultB)
    let taskResult = W.TaskResult version (toJSON response)
    let userdata = []

    pure $ W.WorkerResult taskResult userdata


-- In real application import this code from worker library

exampleAName :: W.WorkerName
exampleAName = W.WorkerName "exampleA"

newtype ExampleAConfig = ExampleAConfig { configA :: String }
  deriving (Generic)
instance FromJSON ExampleAConfig
instance ToJSON ExampleAConfig

newtype ExampleAResult = ExampleAResult { resultA :: Int }
  deriving (Show, Generic)
instance ToJSON ExampleAResult
instance FromJSON ExampleAResult


-- In real application import this code from worker library

exampleBName :: W.WorkerName
exampleBName = W.WorkerName "exampleB"

newtype ExampleBConfig = ExampleBConfig { configB :: Float }
  deriving (Generic)
instance FromJSON ExampleBConfig
instance ToJSON ExampleBConfig

data ExampleBResult = ExampleBResult { resultB :: Float
                                     , stateB  :: Int
                                     }
  deriving (Show, Generic)
instance ToJSON ExampleBResult
instance FromJSON ExampleBResult
