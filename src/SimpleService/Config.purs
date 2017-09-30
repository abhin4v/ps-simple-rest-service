module SimpleService.Config where

import Data.Config
import Prelude

import Control.Monad.Eff (Eff)
import Data.Config.Node (fromEnv)
import Data.Either (Either)
import Data.Set (Set)
import Database.PostgreSQL as PG
import Node.Process (PROCESS)

type ServerConfig =
  { port           :: Int
  , databaseConfig :: PG.PoolConfiguration
  }

databaseConfig :: Config {name :: String} PG.PoolConfiguration
databaseConfig =
  { user: _, password: _, host: _, port: _, database: _, max: _, idleTimeoutMillis: _ }
  <$> string {name: "user"}
  <*> string {name: "password"}
  <*> string {name: "host"}
  <*> int    {name: "port"}
  <*> string {name: "database"}
  <*> int    {name: "pool_size"}
  <*> int    {name: "idle_conn_timeout_millis"}

portConfig :: Config {name :: String} Int
portConfig = int {name: "port"}

serverConfig :: Config {name :: String} ServerConfig
serverConfig =
  { port: _, databaseConfig: _}
  <$> portConfig
  <*> prefix {name: "db"} databaseConfig

readServerConfig :: forall eff.
                    Eff (process :: PROCESS | eff) (Either (Set String) ServerConfig)
readServerConfig = fromEnv "SS" serverConfig
