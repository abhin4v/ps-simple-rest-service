module SimpleService.Middleware.BodyParser where

import Prelude
import Data.Function.Uncurried (Fn3)
import Node.Express.Types (ExpressM, Response, Request)

foreign import jsonBodyParser :: forall e. Fn3 Request Response (ExpressM e Unit) (ExpressM e Unit)
