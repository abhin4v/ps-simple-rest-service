module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Database.PostgreSQL as PG
import Node.Express.Types (EXPRESS)
import SimpleService.Server (runServer)

main :: forall eff. Eff ( console :: CONSOLE
                        , express :: EXPRESS
                        , postgreSQL :: PG.POSTGRESQL
                        | eff) Unit
main = runServer port databaseConfig
  where
    port = 4000
    databaseConfig = { user: "abhinav"
                     , password: ""
                     , host: "localhost"
                     , port: 5432
                     , database: "simple_server"
                     , max: 10
                     , idleTimeoutMillis: 1000
                     }
