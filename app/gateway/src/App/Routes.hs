{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App.Routes
  ( GatewayAPI,
    gatewayAPI,
    gatewayServer,
  )
where

import App.Types
import EulerHS.Prelude
import Kernel.Types.App (FlowServerR)
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
