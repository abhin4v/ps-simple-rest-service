module SimpleService.Validation
  (module MoreExports, module SimpleService.Validation) where

import Prelude

import Control.Monad.Except (ExceptT, except, runExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Node.Express.Handler (HandlerM, Handler)
import Node.Express.Response (sendJson, setStatus)
import Node.Express.Types (EXPRESS)
import Control.Monad.Except (except) as MoreExports

type Validation eff a = ExceptT String (HandlerM (express :: EXPRESS | eff)) a

exceptMaybe :: forall e m a. Applicative m => e -> Maybe a -> ExceptT e m a
exceptMaybe e a = except $ case a of
  Just x  -> Right x
  Nothing -> Left e

exceptCond :: forall e m a. Applicative m => e -> (a -> Boolean) -> a -> ExceptT e m a
exceptCond e cond a = except $ if cond a then Right a else Left e

withValidation :: forall eff a.
                  Validation eff a
               -> (a -> Handler eff)
               -> Handler eff
withValidation action handler = runExceptT action >>= case _ of
  Left err -> do
    setStatus 422
    sendJson {error: err}
  Right x  -> handler x
