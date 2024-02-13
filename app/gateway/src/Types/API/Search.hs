{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Types.API.Search
  ( OnSearchReq,
    SearchReq (..),
    OnSearchAPI,
    SearchAPI,
    onSearchAPI,
    searchAPI,
    RawHeader (getRawHeader),
  )
where

import Data.Aeson (Value)
import qualified Data.Text.Encoding as DT
import EulerHS.Prelude
import Kernel.Types.Beckn.Ack
import Kernel.Utils.Servant.JSONBS
import Servant hiding (Context)
import Types.Beckn.API.Callback
import Types.Beckn.Context

newtype SearchReq = SearchReq
  { context :: Context
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnSearchReq = CallbackReq Value

newtype RawHeader = RawHeader {getRawHeader :: ByteString}

instance FromHttpApiData RawHeader where
  parseUrlPiece = parseHeader . DT.encodeUtf8
  parseQueryParam = parseUrlPiece
  parseHeader = Right . RawHeader

-- These headers added by wai middleware
type SearchAPI =
  Header "Authorization" RawHeader
    :> Header "Beckn-Body-Hash" RawHeader
    :> "search"
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

-- These headers added by wai middleware
type OnSearchAPI =
  Header "Authorization" RawHeader
    :> Header "Beckn-Body-Hash" RawHeader
    :> "on_search"
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] AckResponse

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy
