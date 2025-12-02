{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Product.ProviderRegistry
  ( lookup,
  )
where

import qualified Data.Text as T
import qualified "mock-registry" Domain.Lookup as Registry
import EulerHS.Prelude
import qualified EulerHS.Types as T
import Kernel.Types.Error
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty (prettyShowViaJSON)
import Kernel.Utils.Servant.Client
import Tools.Metrics
import qualified Types.Beckn.Context as B

lookup ::
  ( MonadReader r m,
    MonadFlow m,
    CoreMetrics m,
    HasField "registryUrl" r BaseUrl
  ) =>
  B.Context ->
  m Registry.LookupResponse
lookup context = do
  registryUrl <- asks (.registryUrl)
  let city = fromMaybe context.city $ context.location >>= \location -> fmap (\(B.Descriptor code) -> Just code) $ location.city
  registryFetch
    registryUrl
    Registry.emptyLookupRequest{_type = Just Registry.BPP,
                                domain = Just context.domain,
                                city = city
                               }

registryFetch ::
  ( MonadFlow m,
    CoreMetrics m,
    HasRequestId r,
    MonadReader r m
  ) =>
  BaseUrl ->
  Registry.LookupRequest ->
  m Registry.LookupResponse
registryFetch registryUrl request = do
  logDebug $ "Registry Lookup Request: " <> T.pack (prettyShowViaJSON request)
  callAPI registryUrl (T.client Registry.lookupAPI request) "lookup" Registry.lookupAPI
    >>= fromEitherM (ExternalAPICallError (Just "REGISTRY_CALL_ERROR") registryUrl)
