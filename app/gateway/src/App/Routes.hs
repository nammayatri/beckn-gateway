 {-
 This is the default license template.
 
 File: Routes.hs
 Author: utkarshpandey
 Copyright (c) 2023 utkarshpandey
 
 To edit this license information: Press Ctrl+Shift+P and press 'Create new License Template...'.
-}

module App.Routes
  ( GatewayAPI,
    gatewayAPI,
    gatewayServer,
  )
where

import App.Types
import Kernel.Types.App (FlowServerR)
import EulerHS.Prelude
import qualified Product.Search as Search
import Servant hiding (throwError)
import Types.API.Search

-- TODO: unify these two into one
type HealthAPI =
  "healthz" :> Get '[JSON] Text

type GatewayAPI' =
  "v1"
    :> ( Get '[JSON] Text
           :<|> SearchAPI
           :<|> OnSearchAPI
       )

type GatewayAPI = HealthAPI :<|> GatewayAPI'

gatewayAPI :: Proxy GatewayAPI
gatewayAPI = Proxy

gatewayServer :: FlowServerR AppEnv GatewayAPI
gatewayServer =
  healthHandler :<|> gatewayHandler

healthHandler :: FlowServerR AppEnv HealthAPI
healthHandler =
  pure "UP"

gatewayHandler :: FlowServerR AppEnv GatewayAPI'
gatewayHandler =
  pure "Gateway is UP"
    :<|> Search.search
    :<|> Search.searchCb
