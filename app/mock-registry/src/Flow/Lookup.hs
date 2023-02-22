 {-
 This is the default license template.
 
 File: Lookup.hs
 Author: utkarshpandey
 Copyright (c) 2023 utkarshpandey
 
 To edit this license information: Press Ctrl+Shift+P and press 'Create new License Template...'.
-}

module Flow.Lookup where

import App.Types (FlowHandler)
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Beckn.Ack
import Kernel.Types.Registry.API (LookupRequest, LookupResponse)
import Kernel.Utils.Error (withFlowHandlerAPI)
import Domain.Subscriber
import Storage.Queries.Subscriber as Sub

lookup :: LookupRequest -> FlowHandler LookupResponse
lookup req = withFlowHandlerAPI $ do
  findByAll req.unique_key_id req.subscriber_id req.domain req._type

create :: Subscriber -> FlowHandler AckResponse
create sub = withFlowHandlerAPI $ do
  runTransaction $ Sub.create sub
  return Ack

delete :: Text -> Text -> FlowHandler AckResponse
delete uniqueKeyId subscriberId = withFlowHandlerAPI $ do
  runTransaction $ Sub.deleteByKey (uniqueKeyId, subscriberId)
  return Ack
