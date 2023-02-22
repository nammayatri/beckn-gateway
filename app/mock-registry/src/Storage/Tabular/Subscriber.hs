 {-
 Copyright 2022-23, Juspay India Pvt Ltd
 
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 
 This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
 
 You should have received a copy of the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}


{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Subscriber where

import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Base64
import qualified Kernel.Types.Registry.Subscriber as Domain
import qualified Domain.Subscriber as Domain

derivePersistField "Domain.Domain"

derivePersistField "Domain.SubscriberStatus"

derivePersistField "Domain.SubscriberType"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    SubscriberT sql=subscriber
      uniqueKeyId Text
      subscriberId Text
      subscriberUrl Text
      subscriberType Domain.SubscriberType sql=type
      domain Domain.Domain
      city Text Maybe
      country Text Maybe
      signingPublicKey Base64
      encrPublicKey Base64 Maybe
      validFrom UTCTime Maybe
      validUntil UTCTime Maybe
      status Domain.SubscriberStatus Maybe
      created UTCTime
      updated UTCTime

      Primary uniqueKeyId subscriberId
      deriving Generic
    |]

instance TEntityKey SubscriberT where
  type DomainKey SubscriberT = (Text, Text)
  fromKey (SubscriberTKey keyId subId) = (keyId, subId)
  toKey (keyId, subId) = SubscriberTKey keyId subId

instance TType SubscriberT Domain.Subscriber where
  fromTType SubscriberT {..} = do
    subscriberUrl_ <- parseBaseUrl subscriberUrl
    return $
      Domain.Subscriber
        { unique_key_id = uniqueKeyId,
          subscriber_id = subscriberId,
          subscriber_url = subscriberUrl_,
          signing_public_key = signingPublicKey,
          encr_public_key = encrPublicKey,
          valid_from = validFrom,
          valid_until = validUntil,
          _type = subscriberType,
          ..
        }
  toTType Domain.Subscriber {..} = do
    SubscriberT
      { uniqueKeyId = unique_key_id,
        subscriberId = subscriber_id,
        subscriberUrl = showBaseUrl subscriber_url,
        signingPublicKey = signing_public_key,
        encrPublicKey = encr_public_key,
        validFrom = valid_from,
        validUntil = valid_until,
        subscriberType = _type,
        ..
      }
