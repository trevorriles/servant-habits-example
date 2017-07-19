{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.JS
import System.FilePath

-- Define some types
type Year = Integer
type Month = Integer
type Day = Integer

data LogStatus = Completed | NotCompleted | Skipped deriving (Generic, Show)

data LogItem = LogItem {
  logDate :: (Year, Month, Day),
  logStatus :: LogStatus
} deriving (Generic, Show)

data Habit = Habit {
  habitId :: Integer,
  habitName :: String,
  habitMotivation :: Maybe String,
  habitChain :: Maybe [LogItem]
} deriving (Generic, Show)

instance ToJSON LogStatus
instance ToJSON LogItem
instance ToJSON Habit

instance FromJSON LogStatus
instance FromJSON LogItem
instance FromJSON Habit

-- create empty list
newHabitList :: IO (TVar [Habit])
newHabitList = newTVarIO []

addHabit :: MonadIO m => TVar [Habit] -> Habit -> m [Habit]
addHabit habitList habit = liftIO . atomically $ do
  oldList <- readTVar habitList
  let newList = [habit] ++ oldList
  writeTVar habitList newList
  return newList

getHabitList :: MonadIO m => TVar [Habit] -> m [Habit]
getHabitList habitList = liftIO $ readTVarIO habitList

-- Our API

type HabitApi = "habits" :> ReqBody '[JSON] Habit :> Post '[JSON] [Habit]
           :<|> "habits" :> Get '[JSON] [Habit]

type HabitApi' = HabitApi :<|> Raw

habitApi' :: Proxy HabitApi'
habitApi' = Proxy


-- Handlers

www :: FilePath
www = "www"

server :: TVar [Habit] -> Server HabitApi
server habitList = addHabit habitList
              :<|> getHabitList habitList

server' :: TVar [Habit] -> Server HabitApi'
server' habitList = server habitList
               :<|> serveDirectory www

runServer :: TVar [Habit] -> Int -> IO ()
runServer var port = run port (serve habitApi' $ server' var)

main :: IO ()
main = do
  writeJSForAPI habitApi vanillaJS (www </> "api.js")

  hlist <- newHabitList

  runServer hlist 8080
