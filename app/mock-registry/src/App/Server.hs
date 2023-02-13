module App.Server where

import App.Routes (registryAPI, registryFlow)
import App.Types (Env)
import Kernel.Utils.App (hashBodyForSignature)
import qualified Kernel.Utils.Servant.Server as BU
import EulerHS.Prelude
import Servant (Application, Context (..))

runServer :: Env -> Application
runServer env =
  BU.run registryAPI registryFlow context env
    & hashBodyForSignature
  where
    context = EmptyContext
