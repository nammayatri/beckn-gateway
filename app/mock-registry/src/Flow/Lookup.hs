{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Flow.Lookup where

import App.Types (FlowHandler)
import Domain.Subscriber
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Beckn.Ack
import Kernel.Types.Registry.API (LookupRequest, LookupResponse)
import Kernel.Utils.Error (withFlowHandlerAPI)
import Storage.Queries.Subscriber as Sub

lookup :: LookupRequest -> FlowHandler LookupResponse
lookup req = withFlowHandlerAPI $ do
  findByAll req.unique_key_id req.subscriber_id req.domain req._type req.city

create :: Subscriber -> FlowHandler AckResponse
create sub = withFlowHandlerAPI $ do
  runTransaction $ Sub.create sub
  return Ack

delete :: Text -> Text -> FlowHandler AckResponse
delete uniqueKeyId subscriberId = withFlowHandlerAPI $ do
  runTransaction $ Sub.deleteByKey (uniqueKeyId, subscriberId)
  return Ack
