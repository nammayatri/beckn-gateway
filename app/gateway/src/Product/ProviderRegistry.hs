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

import EulerHS.Prelude
import Kernel.Types.Common
import Kernel.Types.Registry as Registry
import qualified Kernel.Types.Registry.API as Registry
import qualified Kernel.Utils.Registry as Registry
import Tools.Metrics
import qualified Types.Beckn.Context as B

lookup ::
  ( MonadReader r m,
    MonadFlow m,
    Registry m,
    CoreMetrics m,
    HasField "registryUrl" r BaseUrl
  ) =>
  B.Context ->
  m [Registry.Subscriber]
lookup context = do
  registryUrl <- asks (.registryUrl)
  let city = case context.location.city of
        Just (B.Descriptor code) -> Just code
        Nothing -> context.city
  Registry.registryFetch
    registryUrl
    Registry.emptyLookupRequest{_type = Just Registry.BPP,
                                domain = Just context.domain,
                                city = city
                               }
