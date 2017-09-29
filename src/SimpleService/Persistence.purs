module SimpleServer.Persistence
  ( insertUser
  , findUser
  , updateUser
  , deleteUser
  , listUsers
  ) where

import Prelude

import Control.Monad.Aff (Aff)
import Data.Array as Array
import Data.Maybe (Maybe)
import Database.PostgreSQL as PG
import SimpleServer.Types (User(..), UserID)

insertUserQuery :: String
insertUserQuery = "insert into users (id, name) values ($1, $2)"

findUserQuery :: String
findUserQuery = "select id, name from users where id = $1"

updateUserQuery :: String
updateUserQuery = "update users set name = $1 where id = $2"

deleteUserQuery :: String
deleteUserQuery = "delete from users where id = $1"

listUsersQuery :: String
listUsersQuery = "select id, name from users"

insertUser :: forall eff. PG.Connection -> User -> Aff (postgreSQL :: PG.POSTGRESQL | eff) Unit
insertUser conn user = PG.execute conn (PG.Query insertUserQuery) user

findUser :: forall eff. PG.Connection -> UserID -> Aff (postgreSQL :: PG.POSTGRESQL | eff) (Maybe User)
findUser conn userID = map Array.head $ PG.query conn (PG.Query findUserQuery) (PG.Row1 userID)

updateUser :: forall eff. PG.Connection -> User -> Aff (postgreSQL :: PG.POSTGRESQL | eff) Unit
updateUser conn (User {id, name}) = PG.execute conn (PG.Query updateUserQuery) (PG.Row2 name id)

deleteUser :: forall eff. PG.Connection -> UserID -> Aff (postgreSQL :: PG.POSTGRESQL | eff) Unit
deleteUser conn userID = PG.execute conn (PG.Query deleteUserQuery) (PG.Row1 userID)

listUsers :: forall eff. PG.Connection -> Aff (postgreSQL :: PG.POSTGRESQL | eff) (Array User)
listUsers conn = PG.query conn (PG.Query listUsersQuery) PG.Row0
