module SimpleService.Handler where

import Prelude

import Control.Monad.Aff.Class (liftAff)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Foreign (renderForeignError)
import Data.Foreign.Class (encode)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Database.PostgreSQL as PG
import Node.Express.Handler (Handler)
import Node.Express.Request (getBody, getRouteParam)
import Node.Express.Response (end, sendJson, setStatus)
import SimpleService.Persistence as P
import SimpleService.Types

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
        then respondNoContent 204
        else respond 404 { error: "User not found with id: " <> sUserId }

createUser :: forall eff. PG.Pool -> Handler (postgreSQL :: PG.POSTGRESQL | eff)
createUser pool = getBody >>= case _ of
  Left errs -> respond 422 { error: intercalate ", " $ map renderForeignError errs}
  Right u@(User user) ->
    if user.id <= 0
      then respond 422 { error: "User ID must be positive: " <> show user.id}
      else if user.name == ""
        then respond 422 { error: "User name must not be empty" }
        else do
          liftAff (PG.withConnection pool $ flip P.insertUser u)
          respondNoContent 201

updateUser :: forall eff. PG.Pool -> Handler (postgreSQL :: PG.POSTGRESQL | eff)
updateUser pool = getRouteParam "id" >>= case _ of
  Nothing -> respond 422 { error: "User ID is required" }
  Just sUserId -> case fromString sUserId of
    Nothing -> respond 422 { error: "User ID must be positive: " <> sUserId }
    Just userId -> getBody >>= case _ of
      Left errs -> respond 422 { error: intercalate ", " $ map renderForeignError errs}
      Right (UserPatch userPatch) -> case unNullOrUndefined userPatch.name of
        Nothing -> respondNoContent 204
        Just userName -> if userName == ""
          then respond 422 { error: "User name must not be empty" }
          else do
            savedUser <- liftAff $ PG.withConnection pool \conn -> PG.withTransaction conn do
              P.findUser conn userId >>= case _ of
                Nothing -> pure Nothing
                Just (User user) -> do
                  let user' = User (user { name = userName })
                  P.updateUser conn user'
                  pure $ Just user'
            case savedUser of
              Nothing -> respond 404 { error: "User not found with id: " <> sUserId }
              Just user -> respond 200 (encode user)

listUsers :: forall eff. PG.Pool -> Handler (postgreSQL :: PG.POSTGRESQL | eff)
listUsers pool = liftAff (PG.withConnection pool P.listUsers) >>= encode >>> respond 200

respond :: forall eff a. Int -> a -> Handler eff
respond status body = do
  setStatus status
  sendJson body

respondNoContent :: forall eff. Int -> Handler eff
respondNoContent status = do
  setStatus status
  end
