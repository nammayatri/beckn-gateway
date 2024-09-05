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
  )
where

import App.Types
import qualified Data.Aeson as A
import qualified Data.Text as T
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import Kernel.Types.Beckn.Ack
import Kernel.Types.Error
import qualified Kernel.Types.Registry.Subscriber as Subscriber
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (callBecknAPI')
import Kernel.Utils.Servant.BaseUrl
import Kernel.Utils.Servant.Client
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..), authCheck, signatureAuthManagerKey)
import qualified Product.ProviderRegistry as BP
import qualified Types.API.Gateway.Search as ExternalAPI
import Types.API.Search (RawHeader (getRawHeader), SearchReq (..))
import Types.Error
import Utils.Common

search ::
  Maybe RawHeader ->
  Maybe RawHeader ->
  ByteString ->
  FlowHandler AckResponse
search mbSignPayloadHeader mbBodyHashHeader rawReq = withFlowHandlerBecknAPI' do
  req :: SearchReq <- rawReq & A.eitherDecodeStrict & fromEitherM (InvalidRequest . T.pack)
  withTransactionIdLogTag req $ do
    let gatewaySearchSignAuth = ET.client ExternalAPI.searchAPI
        context = req.context
    (SignatureAuthResult proxySign _) <- withLogTag "gateway authCheck" $ do
      let headerNameStr = "Authorization"
          domain = req.context.domain
          subscriberType = Subscriber.BAP
      logDebug $ "SubscriberType: " <> show subscriberType <> "; domain: " <> show domain
      -- FIXME merchantId not required for gateway
      authCheck headerNameStr (getRawHeader <$> mbSignPayloadHeader) (getRawHeader <$> mbBodyHashHeader) "merchantId" subscriberType domain
    providers <- BP.lookup context
    internalEndPointHashMap <- asks (.internalEndPointHashMap)
    when (null providers) $ throwError NoProviders
    forM_ providers $ \provider -> fork "Provider search" . withLogTag "search_req" $ do
      let providerUrl = provider.subscriber_url
      withLogTag ("providerUrl_" <> showBaseUrlText providerUrl) . withShortRetry $
        -- TODO maybe we should explicitly call sign request here instead of using callAPIWithTrail'?
        void $
          callBecknAPI'
            (Just $ ET.ManagerSelector signatureAuthManagerKey)
            Nothing
            (Just internalEndPointHashMap)
            providerUrl
            (gatewaySearchSignAuth (Just proxySign) rawReq)
            "search"
            ExternalAPI.searchAPI
    return Ack
