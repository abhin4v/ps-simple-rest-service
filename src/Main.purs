module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Data.Either (Either(..))
import Data.Set (toUnfoldable)
import Data.String (joinWith)
import Database.PostgreSQL as PG
import Node.Express.Types (EXPRESS)
import Node.Process (PROCESS)
import Node.Process as Process
import SimpleService.Config (readServerConfig)
import SimpleService.Logger as Log
import SimpleService.Server (runServer)

main :: forall eff. Eff ( console :: CONSOLE
                        , express :: EXPRESS
                        , postgreSQL :: PG.POSTGRESQL
                        , process :: PROCESS
                        , now :: NOW
                        | eff) Unit
main = readServerConfig >>= case _ of
  Left missingKeys -> do
    Log.error $ "Unable to start. Missing Env keys: " <> joinWith ", " (toUnfoldable missingKeys)
    Process.exit 1
  Right { port, databaseConfig } -> runServer port databaseConfig
