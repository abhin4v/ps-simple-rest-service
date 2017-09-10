module SimpleService.Server (runServer) where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Control.Monad.Eff.Exception (catchException)
import Database.PostgreSQL as PG
import Node.Express.App (App, delete, get, listenHttp)
import Node.Express.Types (EXPRESS)
import SimpleService.Handler (deleteUser, getUser)

app :: forall eff. PG.Pool -> App (postgreSQL :: PG.POSTGRESQL | eff)
app pool = do
  get "/v1/user/:id" $ getUser pool
  delete "/v1/user/:id" $ deleteUser pool

runServer :: forall eff.
             Int
          -> PG.PoolConfiguration
          -> Eff ( express :: EXPRESS
                 , postgreSQL :: PG.POSTGRESQL
                 , console :: CONSOLE
                 | eff ) Unit
runServer port databaseConfig = catchException logShow $
  void $ launchAff do
    pool <- PG.newPool databaseConfig
    let app' = app pool
    liftEff $ listenHttp app' port \_ -> log $ "Server listening on :" <> show port
