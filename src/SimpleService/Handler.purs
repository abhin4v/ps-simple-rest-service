module SimpleService.Handler where

import Prelude

import Control.Monad.Aff.Class (liftAff)
import Data.Foreign.Class (encode)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Database.PostgreSQL as PG
import Node.Express.Handler (Handler)
import Node.Express.Request (getRouteParam)
import Node.Express.Response (sendJson, setStatus)
import SimpleServer.Persistence as P

getUser :: forall eff. PG.Pool -> Handler (postgreSQL :: PG.POSTGRESQL | eff)
getUser pool = getRouteParam "id" >>= case _ of
  Nothing -> respond 422 { error: "User ID is required" }
  Just sUserId -> case fromString sUserId of
    Nothing -> respond 422 { error: "User ID must be an integer: " <> sUserId }
    Just userId -> liftAff (PG.withConnection pool $ flip P.findUser userId) >>= case _ of
      Nothing -> respond 404 { error: "User not found with id: " <> sUserId }
      Just user -> respond 200 (encode user)

deleteUser :: forall eff. PG.Pool -> Handler (postgreSQL :: PG.POSTGRESQL | eff)
deleteUser pool = getRouteParam "id" >>= case _ of
  Nothing -> respond 422 { error: "User ID is required" }
  Just sUserId -> case fromString sUserId of
    Nothing -> respond 422 { error: "User ID must be an integer: " <> sUserId }
    Just userId -> do
      found <- liftAff $ PG.withConnection pool \conn -> PG.withTransaction conn do
        P.findUser conn userId >>= case _ of
          Nothing -> pure false
          Just _  -> do
            P.deleteUser conn userId
            pure true
      if found
        then respond 204 {}
        else respond 404 { error: "User not found with id: " <> sUserId }

respond :: forall eff a. Int -> a -> Handler eff
respond status body = do
  setStatus status
  sendJson body
