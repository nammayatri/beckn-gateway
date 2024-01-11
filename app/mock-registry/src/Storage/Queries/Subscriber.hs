{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Subscriber where

import Domain.Subscriber
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Beckn.City as Context
import Kernel.Types.Beckn.Domain as Context
import Kernel.Types.Common
import Kernel.Utils.Logging
import Storage.Tabular.Subscriber

findByAll' :: (MonadThrow m, Log m, Transactionable m) => Maybe Text -> Maybe Text -> Maybe Context.Domain -> Maybe SubscriberType -> m [Subscriber]
findByAll' mbKeyId mbSubId mbDomain mbSubType =
  Esq.findAll $ do
    parkingLocation <- from $ table @SubscriberT
    where_ $
      whenJust_ mbKeyId (\keyId -> parkingLocation ^. SubscriberUniqueKeyId ==. val keyId)
        &&. whenJust_ mbSubId (\subId -> parkingLocation ^. SubscriberSubscriberId ==. val subId)
        &&. whenJust_ mbDomain (\domain -> parkingLocation ^. SubscriberDomain ==. val domain)
        &&. whenJust_ mbSubType (\subType -> parkingLocation ^. SubscriberSubscriberType ==. val subType)
    return parkingLocation

findByAll :: (MonadThrow m, Log m, Transactionable m) => Maybe Text -> Maybe Text -> Maybe Context.Domain -> Maybe SubscriberType -> Maybe City -> m [Subscriber]
findByAll mbKeyId mbSubId mbDomain mbSubType mbCity = do
  subscribers <- findByAll' mbKeyId mbSubId mbDomain mbSubType
  logInfo ("FindByAll in registry before city result is :  " <> show subscribers)
  return (maybe subscribers (\city -> filterByCity subscribers city) mbCity)
  where
    filterByCity subscribers city =
      filter (\subscriber -> elem city subscriber.city) subscribers

create :: Subscriber -> SqlDB ()
create = Esq.create

deleteByKey :: (Text, Text) -> SqlDB ()
deleteByKey = Esq.deleteByKey @SubscriberT

findAll :: (MonadThrow m, Log m, Transactionable m) => m [Subscriber]
findAll =
  Esq.findAll $ do
    from $ table @SubscriberT
