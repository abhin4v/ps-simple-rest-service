module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Either (Either(..))
import Data.Set (toUnfoldable)
import Data.String (joinWith)
import Database.PostgreSQL as PG
import Node.Express.Types (EXPRESS)
import Node.Process (PROCESS)
import Node.Process as Process
import SimpleService.Config (readServerConfig)
import SimpleService.Server (runServer)

main :: forall eff. Eff ( console :: CONSOLE
                        , express :: EXPRESS
                        , postgreSQL :: PG.POSTGRESQL
                        , process :: PROCESS
                        | eff) Unit
main = readServerConfig >>= case _ of
  Left missingKeys -> do
    log $ "Unable to start. Missing Env keys: " <> joinWith ", " (toUnfoldable missingKeys)
    Process.exit 1
  Right { port, databaseConfig } -> runServer port databaseConfig
