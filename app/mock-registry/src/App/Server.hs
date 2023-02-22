 {-
 This is the default license template.
 
 File: Server.hs
 Author: utkarshpandey
 Copyright (c) 2023 utkarshpandey
 
 To edit this license information: Press Ctrl+Shift+P and press 'Create new License Template...'.
-}

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
