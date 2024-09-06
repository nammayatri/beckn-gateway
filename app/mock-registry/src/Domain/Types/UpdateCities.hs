{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.UpdateCities where

import Data.OpenApi (ToSchema)
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Registry.Subscriber (SubscriberType (..))
import Kernel.Utils.JSON (removeNullFields)
import Servant

data UpdateCitiesReq = UpdateCitiesReq
  { uniqueKeyId :: Text,
    subscriberId :: Text,
    subscriberType :: SubscriberType,
    domain :: Context.Domain,
    appendCities :: Maybe [Text],
    replaceCities :: Maybe [Text]
  }
  deriving (Show, Generic, FromJSON, ToSchema)

instance ToJSON UpdateCitiesReq where
  toJSON = genericToJSON removeNullFields

data UpdateCitiesRes = UpdateCitiesRes
  { added :: Maybe [Text],
    removed :: Maybe [Text]
  }
  deriving (Show, Generic, FromJSON, ToSchema)

instance ToJSON UpdateCitiesRes where
  toJSON = genericToJSON removeNullFields

type UpdateCitiesAPI =
  "updateCities"
    :> Header "x-api-key" Text
    :> ReqBody '[JSON] UpdateCitiesReq
    :> Post '[JSON] UpdateCitiesRes

updateCitiesAPI :: Proxy UpdateCitiesAPI
updateCitiesAPI = Proxy
