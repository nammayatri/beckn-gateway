{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Product.Search
  ( search,
    searchCb,
  )
where

import App.Types
import qualified Data.Aeson as A
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (callBecknAPI')
import Kernel.Utils.Servant.BaseUrl
import Kernel.Utils.Servant.Client
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..), signatureAuthManagerKey)
import qualified Product.ProviderRegistry as BP
import qualified Types.API.Gateway.Search as ExternalAPI
import Types.API.Search (OnSearchReq, SearchReq (..))
import Types.Error
import Utils.Common

search ::
  SignatureAuthResult ->
  ByteString ->
  FlowHandler AckResponse
search (SignatureAuthResult proxySign _) rawReq = withFlowHandlerBecknAPI do
  req :: SearchReq <- rawReq & A.eitherDecodeStrict & fromEitherM (InvalidRequest . T.pack)
  withTransactionIdLogTag req $ do
    let gatewaySearchSignAuth = ET.client ExternalAPI.searchAPI
        context = req.context
    providers <- BP.lookup context
    when (null providers) $ throwError NoProviders
    forM_ providers $ \provider -> fork "Provider search" . withLogTag "search_req" $ do
      let providerUrl = provider.subscriber_url
      withLogTag ("providerUrl_" <> showBaseUrlText providerUrl) . withShortRetry $
        -- TODO maybe we should explicitly call sign request here instead of using callAPIWithTrail'?
        void $
          callBecknAPI'
            (Just $ ET.ManagerSelector signatureAuthManagerKey)
            Nothing
            providerUrl
            (gatewaySearchSignAuth (Just proxySign) rawReq)
            "search"
            ExternalAPI.searchAPI
    return Ack

searchCb ::
  SignatureAuthResult ->
  ByteString ->
  FlowHandler AckResponse
searchCb (SignatureAuthResult proxySign _subscriber) rawReq = withFlowHandlerBecknAPI do
  req :: OnSearchReq <- rawReq & A.eitherDecodeStrict & fromEitherM (InvalidRequest . T.pack)
  withTransactionIdLogTag req . withLogTag "search_cb" $ do
    -- TODO: source providerUrl from _subscriber
    providerUrl <- req.context.bpp_uri & fromMaybeM (InvalidRequest "Missing context.bpp_uri")
    withLogTag ("providerUrl_" <> showBaseUrlText providerUrl) do
      let gatewayOnSearchSignAuth = ET.client ExternalAPI.onSearchAPI
      let bapUri = req.context.bap_uri
      void . withShortRetry $
        callBecknAPI'
          (Just $ ET.ManagerSelector signatureAuthManagerKey)
          Nothing
          bapUri
          (gatewayOnSearchSignAuth (Just proxySign) rawReq)
          "on_search"
          ExternalAPI.onSearchAPI
      return Ack
