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
  habitMotivation :: String,
  habitChain :: [LogItem]
} deriving (Generic, Show)

-- Create a mutable variable to store our data in. Instead of a DB.
type HabitList = [Habit]

instance ToJSON LogStatus
instance ToJSON LogItem
instance ToJSON Habit
instance ToJSON HabitList

-- create empty list
newHabitList :: IO (TVar [Habit])
newHabitList = newTVarIO []

addHabit :: MonadIO m => TVar HabitList -> Habit -> m HabitList
addHabit habitList habit = liftIO . automically $ do
  oldList <- readTVar habitList
  let newList = habit ++ oldList
  writeTVar habitList newList
  return newList

getHabitList :: MonadIO m => TVar HabitList -> m HabitList
getHabitList habitList = liftIO $ readTVarIO habitList

-- Our API

type HabitApi = "habits" :> ReqBody '[JSON] Habit :> Post '[JSON] HabitList
           :<|> "habits" :> Get '[JSON] HabitList

type HabitApi' = HabitApi :<|> Raw

habitApi :: Proxy HabitApi

habitApi = Proxy

habitApi' :: Proxy HabitApi'
habitApi' = Proxy


-- Handlers

www :: FilePath
www = "www"

server :: TVar HabitList -> Server HabitApi
server habitList = addHabit habitList
              :<|> getHabitList habitList

server' :: TVar HabitList -> Server HabitApi'
server' habitList = server habitList
               :<|> serveDirectory www

runServer :: TVar HabitList -> Int -> IO ()
runServer var port = run port (serve habitApi' $ server' var)

main :: IO ()
main = do
  writeJSForAPI habitApi vanillaJS (www </> "api.js")

  hlist <- newHabitList

  runServer hlist 8080
