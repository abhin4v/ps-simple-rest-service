module SimpleService.Types where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Database.PostgreSQL (class FromSQLRow, class ToSQLRow, fromSQLValue, toSQLValue)

type UserID = Int

newtype User = User
  { id   :: UserID
  , name :: String
  }

derive instance genericUser :: Generic User _

instance showUser :: Show User where
  show = genericShow

instance decodeUser :: Decode User where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }

instance encodeUser :: Encode User where
  encode = genericEncode $ defaultOptions { unwrapSingleConstructors = true }

instance userFromSQLRow :: FromSQLRow User where
  fromSQLRow [id, name] =
    User <$> ({ id: _, name: _} <$> fromSQLValue id <*> fromSQLValue name)

  fromSQLRow xs = Left $ "Row has " <> show n <> " fields, expecting 2."
    where n = Array.length xs

instance userToSQLRow :: ToSQLRow User where
  toSQLRow (User {id, name}) = [toSQLValue id, toSQLValue name]

newtype UserPatch = UserPatch { name :: NullOrUndefined String }

derive instance genericUserPatch :: Generic UserPatch _

instance decodeUserPatch :: Decode UserPatch where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
