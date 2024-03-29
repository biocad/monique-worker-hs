{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON (..), ToJSON (..))
import           GHC.Generics           (Generic)
import qualified Network.Monique.Worker as W (Algo, TaskResult (..),
                                              WorkerName (..),
                                              WorkerResult (..), runApp,
                                              throwWorkerErrorI)

main :: IO ()
main = W.runApp exampleAName () exampleAProcess

newtype ExampleAConfig = ExampleAConfig { configA :: String }
  deriving (Generic)
instance FromJSON ExampleAConfig
instance ToJSON ExampleAConfig

newtype ExampleAResult = ExampleAResult { resultA :: Int }
  deriving (Show, Generic)

instance ToJSON ExampleAResult

version :: Int
version = 1

exampleAName :: W.WorkerName
exampleAName = W.WorkerName "exampleA"

exampleAProcess :: W.Algo ExampleAConfig ()
exampleAProcess workerInfo (ExampleAConfig configA') = do
    liftIO $ print configA'
    let userdata' = [("userdataExample", toJSON configA')]
    let length' = length configA'
    if length' < 10
      then W.throwWorkerErrorI workerInfo "Length is too small" -- this is how to throw error
      else do
        let response = ExampleAResult length'
        let taskResult = W.TaskResult version (toJSON response)
        pure $ W.WorkerResult taskResult userdata'


