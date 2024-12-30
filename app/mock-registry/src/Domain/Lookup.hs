{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Lookup
  ( LookupRequest (..),
    SubscriberType (..),
    LookupResponse,
    LookupAPI,
    emptyLookupRequest,
    lookupAPI,
  )
where

import Data.OpenApi (ToSchema)
import Domain.Subscriber (Subscriber)
import EulerHS.Prelude
import Kernel.Types.Beckn.Country (Country)
import Kernel.Types.Beckn.Domain (Domain)
import Kernel.Types.Registry.Subscriber (SubscriberType (..))
import Kernel.Utils.JSON (stripPrefixUnderscoreIfAny)
import Servant

type LookupAPI =
  "lookup"
    :> ReqBody '[JSON] LookupRequest
    :> Post '[JSON] LookupResponse

lookupAPI :: Proxy LookupAPI
lookupAPI = Proxy

data LookupRequest = LookupRequest
  { unique_key_id :: Maybe Text,
    subscriber_id :: Maybe Text,
    _type :: Maybe SubscriberType,
    domain :: Maybe Domain,
    country :: Maybe Country,
    city :: Maybe Text
  }
  deriving (Show, Generic, ToSchema)

emptyLookupRequest :: LookupRequest
emptyLookupRequest = LookupRequest Nothing Nothing Nothing Nothing Nothing Nothing

instance FromJSON LookupRequest where
  parseJSON = genericParseJSON stripPrefixUnderscoreIfAny

instance ToJSON LookupRequest where
  toJSON = genericToJSON stripPrefixUnderscoreIfAny

type LookupResponse = [Subscriber]
