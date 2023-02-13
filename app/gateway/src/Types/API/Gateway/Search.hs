module Types.API.Gateway.Search where

import Kernel.Types.Beckn.Ack
import Kernel.Utils.Servant.JSONBS
import Kernel.Utils.SignatureAuth
import EulerHS.Prelude
import Servant

type SearchAPI =
  "search"
    :> Header "Authorization" SignaturePayload
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

type OnSearchAPI =
  "on_search"
    :> Header "Authorization" SignaturePayload
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] AckResponse

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy
