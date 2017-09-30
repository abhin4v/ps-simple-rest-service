module SimpleService.Server (runServer) where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Now (NOW)
import Data.Either (fromRight)
import Data.Maybe (maybe)
import Data.String (toUpper)
import Data.String.Regex (Regex, regex) as Re
import Data.String.Regex.Flags (noFlags) as Re
import Database.PostgreSQL as PG
import Node.Express.App (App, all, delete, get, http, listenHttp, post, use, useExternal, useOnError)
import Node.Express.Handler (Handler, next)
import Node.Express.Request (getMethod, getPath)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Types (EXPRESS, Method(..))
import Partial.Unsafe (unsafePartial)
import SimpleService.Handler (createUser, deleteUser, getUser, listUsers, updateUser)
import SimpleService.Logger as Log
import SimpleService.Middleware.BodyParser (jsonBodyParser)

allRoutePattern :: Re.Regex
allRoutePattern = unsafePartial $ fromRight $ Re.regex "/.*" Re.noFlags

requestLogger :: forall eff. Handler (console :: CONSOLE, now :: NOW | eff)
requestLogger = do
  method <- getMethod
  path   <- getPath
  Log.debug $ "HTTP: " <> maybe "" id ((toUpper <<< show) <$> method) <> " " <> path
  next

app :: forall eff. PG.Pool
                -> App (postgreSQL :: PG.POSTGRESQL, console :: CONSOLE, now :: NOW | eff)
app pool = do
  useExternal jsonBodyParser
  use requestLogger

  get "/v1/user/:id"    $ getUser pool
  delete "/v1/user/:id" $ deleteUser pool
  post "/v1/users"      $ createUser pool
  patch "/v1/user/:id"  $ updateUser pool
  get "/v1/users"       $ listUsers pool

  all allRoutePattern do
    setStatus 404
    sendJson {error: "Route not found"}

  useOnError \err -> do
    Log.error $ "Uncaught error in handler: " <> show err
    setStatus 500
    sendJson {error: message err}
  where
    patch = http (CustomMethod "patch")

runServer :: forall eff.
             Int
          -> PG.PoolConfiguration
          -> Eff ( express :: EXPRESS
                 , postgreSQL :: PG.POSTGRESQL
                 , console :: CONSOLE
                 , now :: NOW
                 | eff ) Unit
runServer port databaseConfig =
  void $ runAff (\err -> Log.error $ "Error in running server: " <> show err) pure do
    pool <- PG.newPool databaseConfig
    let app' = app pool
    void $ liftEff $ listenHttp app' port \_ -> Log.info $ "Server listening on :" <> show port
