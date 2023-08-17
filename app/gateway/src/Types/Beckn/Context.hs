{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Types.Beckn.Context where

import Data.Aeson
import Data.Text
import EulerHS.Prelude
import Kernel.Types.Beckn.City
import Kernel.Types.Beckn.Country
import Kernel.Types.Beckn.Domain
import Servant.Client (BaseUrl)

data Context = Context
  { domain :: Domain,
    bap_uri :: BaseUrl,
    bpp_uri :: Maybe BaseUrl,
    transaction_id :: Maybe Text,
    city :: Maybe City,
    country :: Maybe Country
  }
  deriving (Generic, FromJSON, Show)

instance ToJSON Context where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}
