module SimpleService.Logger
  ( debug
  , info
  , warn
  , error
  ) where

import Prelude

import Control.Logger as L
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console as C
import Control.Monad.Eff.Now (NOW, now)
import Data.DateTime.Instant (toDateTime)
import Data.Either (fromRight)
import Data.Formatter.DateTime (Formatter, format, parseFormatString)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.String (toUpper)
import Partial.Unsafe (unsafePartial)

data Level = Debug | Info | Warn | Error

derive instance eqLevel :: Eq Level
derive instance ordLevel :: Ord Level
derive instance genericLevel :: Generic Level _

instance showLevel :: Show Level where
  show = toUpper <<< genericShow

type Entry =
  { level   :: Level
  , message :: String
  }

dtFormatter :: Formatter
dtFormatter = unsafePartial $ fromRight $ parseFormatString "YYYY-MM-DD HH:mm:ss.SSS"

logger :: forall m e.
          (MonadEff (console :: C.CONSOLE, now :: NOW | e) m) => L.Logger m Entry
logger = L.Logger $ \{ level, message } -> liftEff do
  time <- toDateTime <$> now
  C.log $ "[" <> format dtFormatter time <> "] " <> show level <> " " <> message

log :: forall m e.
        MonadEff (console :: C.CONSOLE , now :: NOW | e) m
     => Entry -> m Unit
log entry@{level} = L.log (L.cfilter (\e -> e.level == level) logger) entry

debug :: forall m e.
         MonadEff (console :: C.CONSOLE , now :: NOW | e) m => String -> m Unit
debug message = log { level: Debug, message }

info :: forall m e.
        MonadEff (console :: C.CONSOLE , now :: NOW | e) m => String -> m Unit
info message = log { level: Info, message }

warn :: forall m e.
        MonadEff (console :: C.CONSOLE , now :: NOW | e) m => String -> m Unit
warn message = log { level: Warn, message }

error :: forall m e.
         MonadEff (console :: C.CONSOLE , now :: NOW | e) m => String -> m Unit
error message = log { level: Error, message }
