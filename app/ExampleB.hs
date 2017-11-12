{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (get, put)
import           Data.Aeson             (FromJSON (..), ToJSON (..))
import           GHC.Generics           (Generic)
import qualified Network.Monique.Worker as W (Algo, TaskResult (..),
                                              WorkerName (..),
                                              WorkerResult (..), runApp)

main :: IO ()
main = W.runApp exampleBName initialState exampleBProcess
  where initialState = 0

newtype ExampleBConfig = ExampleBConfig { configB :: Float }
  deriving (Generic)
instance FromJSON ExampleBConfig
instance ToJSON ExampleBConfig

data ExampleBResult = ExampleBResult { resultB :: Float
                                     , stateB  :: Int
                                     }
  deriving (Show, Generic)

instance ToJSON ExampleBResult

version :: Int
version = 1

exampleBName :: W.WorkerName
exampleBName = W.WorkerName "exampleB"

exampleBProcess :: W.Algo ExampleBConfig Int
exampleBProcess _ (ExampleBConfig configB') = do
    liftIO $ print configB'

    -- how to work with state
    currentState <- get
    let newState = succ currentState
    put newState

    let response = ExampleBResult (sin configB') newState
    let taskResult = W.TaskResult version (toJSON response)
    let userdata = []

    pure $ W.WorkerResult taskResult userdata


