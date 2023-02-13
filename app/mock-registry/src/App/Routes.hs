 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License 
 
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program 
 
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of 
 
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App.Routes where

import App.Types (FlowServer)
import Kernel.Types.App (MandatoryQueryParam)
import Kernel.Types.Beckn.Ack
import Kernel.Types.Registry.Routes (LookupAPI)
import Domain.Subscriber
import EulerHS.Prelude
import qualified Flow.Lookup as Flow
import Servant

type RegistryAPI =
  LookupAPI
    :<|> CreateAPI
    :<|> DeleteAPI

registryFlow :: FlowServer RegistryAPI
registryFlow = lookupFlow :<|> Flow.create :<|> Flow.delete

registryAPI :: Proxy RegistryAPI
registryAPI = Proxy

lookupFlow :: FlowServer LookupAPI
lookupFlow = Flow.lookup

type CreateAPI =
  "create"
    :> ReqBody '[JSON] Subscriber
    :> Post '[JSON] AckResponse

type DeleteAPI =
  "delete"
    :> MandatoryQueryParam "unique_key_id" Text
    :> MandatoryQueryParam "subscriber_id" Text
    :> Post '[JSON] AckResponse
