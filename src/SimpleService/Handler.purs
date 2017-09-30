module SimpleService.Handler where

import Prelude
import SimpleService.Types

import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Foreign (ForeignError, renderForeignError)
import Data.Foreign.Class (encode)
import Data.Foreign.NullOrUndefined (unNullOrUndefined)
import Data.Int (fromString)
import Data.List.NonEmpty (toList)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Database.PostgreSQL as PG
import Node.Express.Handler (Handler)
import Node.Express.Request (getBody, getRouteParam)
import Node.Express.Response (end, sendJson, setStatus)
import SimpleService.Logger as Log
import SimpleService.Persistence as P
import SimpleService.Validation as V

getUser :: forall eff. PG.Pool
        -> Handler (postgreSQL :: PG.POSTGRESQL, console :: CONSOLE, now :: NOW | eff)
getUser pool = V.withValidation getUserId \userId ->
  liftAff (PG.withConnection pool $ flip P.findUser userId) >>= case _ of
    Nothing -> respond 404 { error: "User not found with id: " <> show userId }
    Just user -> respond 200 (encode user)

deleteUser :: forall eff. PG.Pool
           -> Handler (postgreSQL :: PG.POSTGRESQL, console :: CONSOLE, now :: NOW | eff)
deleteUser pool = V.withValidation getUserId \userId -> do
  found <- liftAff $ PG.withConnection pool \conn -> PG.withTransaction conn do
    P.findUser conn userId >>= case _ of
      Nothing -> pure false
      Just _  -> do
        P.deleteUser conn userId
        Log.debug $ "Deleted user " <> show userId
        pure true
  if found
    then respondNoContent 204
    else respond 404 { error: "User not found with id: " <> show userId }

createUser :: forall eff. PG.Pool
           -> Handler (postgreSQL :: PG.POSTGRESQL, console :: CONSOLE, now :: NOW | eff)
createUser pool = V.withValidation getUser \user@(User {id}) -> do
  liftAff (PG.withConnection pool $ flip P.insertUser user)
  Log.debug $ "Created user " <> show id
  respondNoContent 201
  where
    getUser = lift getBody
      >>= V.except <<< renderForeignErrors
      >>= V.exceptCond "User ID must be positive" (\(User user) -> user.id > 0)
      >>= V.exceptCond "User name must not be empty" (\(User user) -> user.name /= "")

updateUser :: forall eff. PG.Pool
           -> Handler (postgreSQL :: PG.POSTGRESQL, console :: CONSOLE, now :: NOW | eff)
updateUser pool = V.withValidation (Tuple <$> getUserId <*> getUserPatch)
                                   \(Tuple userId (UserPatch userPatch)) ->
    case unNullOrUndefined userPatch.name of
      Nothing -> respondNoContent 204
      Just uName -> V.withValidation (getUserName uName) \userName -> do
        savedUser <- liftAff $ PG.withConnection pool \conn -> PG.withTransaction conn do
          P.findUser conn userId >>= case _ of
            Nothing -> pure Nothing
            Just (User user) -> do
              let user' = User (user { name = userName })
              P.updateUser conn user'
              Log.debug $ "Updated user " <> show userId
              pure $ Just user'
        case savedUser of
          Nothing -> respond 404 { error: "User not found with id: " <> show userId }
          Just user -> respond 200 (encode user)
  where
    getUserPatch = lift getBody >>= V.except <<< renderForeignErrors
    getUserName = V.exceptCond "User name must not be empty" (_ /= "")

listUsers :: forall eff. PG.Pool
          -> Handler (postgreSQL :: PG.POSTGRESQL, console :: CONSOLE, now :: NOW | eff)
listUsers pool = liftAff (PG.withConnection pool P.listUsers) >>= encode >>> respond 200

getUserId :: forall eff. V.Validation eff Int
getUserId = lift (getRouteParam "id")
  >>= V.exceptMaybe "User ID is required"
  >>= fromString >>> V.exceptMaybe "User ID must be an integer"
  >>= V.exceptCond "User ID must be positive" (_ > 0)

renderForeignErrors :: forall a. Either (NonEmptyList ForeignError) a -> Either String a
renderForeignErrors = lmap (toList >>> map renderForeignError >>> intercalate ", ")

respond :: forall eff a. Int -> a -> Handler eff
respond status body = do
  setStatus status
  sendJson body

respondNoContent :: forall eff. Int -> Handler eff
respondNoContent status = do
  setStatus status
  end
