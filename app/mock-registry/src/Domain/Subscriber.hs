{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Subscriber
  ( Subscriber (..),
    SubscriberStatus (..),
    SubscriberType (..),
  )
where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import Data.Time (UTCTime)
import EulerHS.Prelude
import Kernel.Types.Base64
import Kernel.Types.Beckn.Country (Country)
import Kernel.Types.Beckn.Domain (Domain)
import Kernel.Types.Registry (SubscriberStatus (..), SubscriberType (..))
import Servant.Client (BaseUrl)

data Subscriber = Subscriber
  { unique_key_id :: Text,
    subscriber_id :: Text,
    subscriber_url :: BaseUrl,
    _type :: SubscriberType,
    domain :: Domain,
    city :: [Text],
    country :: Maybe Country,
    signing_public_key :: Base64,
    encr_public_key :: Maybe Base64,
    valid_from :: Maybe UTCTime,
    valid_until :: Maybe UTCTime,
    status :: Maybe SubscriberStatus,
    created :: UTCTime,
    updated :: UTCTime
  }
  deriving (Show, Generic)

instance FromJSON Subscriber where
  parseJSON (Object obj) = do
    unique_key_id <- obj .: "ukId"
    _type <- obj .: "type"
    subscriber_id <- obj .: "subscriber_id"
    subscriber_url <- obj .: "subscriber_url"
    domain <- obj .: "domain"
    country <- obj .: "country"
    signing_public_key <- obj .: "signing_public_key"
    encr_public_key <- obj .: "encr_public_key"
    valid_from <- obj .: "valid_from"
    valid_until <- obj .: "valid_until"
    status <- obj .: "status"
    updated <- obj .: "updated"
    created <- obj .: "created"
    city' <- obj .: "city"
    let city = parseCities city'
    pure Subscriber {..}
    where
      parseCities = map T.strip . T.splitOn ","
  parseJSON wrongVal = typeMismatch "Object Subscriber" wrongVal

instance ToJSON Subscriber where
  toJSON Subscriber {..} = do
    object
      [ "ukId" .= unique_key_id,
        "type" .= _type,
        "subscriber_id" .= subscriber_id,
        "subscriber_url" .= subscriber_url,
        "domain" .= domain,
        "country" .= country,
        "signing_public_key" .= signing_public_key,
        "encr_public_key" .= encr_public_key,
        "valid_from" .= valid_from,
        "valid_until" .= valid_until,
        "status" .= status,
        "updated" .= updated,
        "created" .= created,
        "city" .= toCity city
      ]
    where
      toCity = String . T.intercalate ","
