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
import Kernel.Types.Beckn.Domain as Context
import Kernel.Types.Common
import Storage.Tabular.Subscriber

findAllBy' :: (MonadThrow m, Log m, Transactionable m) => Maybe Text -> Maybe Text -> Maybe Context.Domain -> Maybe SubscriberType -> m [Subscriber]
findAllBy' mbKeyId mbSubId mbDomain mbSubType =
  Esq.findAll $ do
    subscriber <- from $ table @SubscriberT
    where_ $
      whenJust_ mbKeyId (\keyId -> subscriber ^. SubscriberUniqueKeyId ==. val keyId)
        &&. whenJust_ mbSubId (\subId -> subscriber ^. SubscriberSubscriberId ==. val subId)
        &&. whenJust_ mbDomain (\domain -> subscriber ^. SubscriberDomain ==. val domain)
        &&. whenJust_ mbSubType (\subType -> subscriber ^. SubscriberSubscriberType ==. val subType)
    return subscriber

findAllBy :: (MonadThrow m, Log m, Transactionable m) => Maybe Text -> Maybe Text -> Maybe Context.Domain -> Maybe SubscriberType -> Maybe Text -> m [Subscriber]
findAllBy mbKeyId mbSubId mbDomain mbSubType mbCity = do
  subscribers <- findAllBy' mbKeyId mbSubId mbDomain mbSubType
  return (maybe subscribers (`filterByCity` subscribers) mbCity)
  where
    filterByCity city = filter (\subscriber -> elem city subscriber.city || elem allCities subscriber.city || city == allCities)

    allCities :: Text
    allCities = "*"

updateCities :: Text -> Text -> Context.Domain -> SubscriberType -> [Text] -> SqlDB ()
updateCities ukId subId domain subType cities = do
  Esq.update $ \tbl -> do
    set
      tbl
      [SubscriberCity =. val (PostgresList cities)]
    where_ $
      tbl ^. SubscriberUniqueKeyId ==. val ukId
        &&. tbl ^. SubscriberSubscriberId ==. val subId
        &&. tbl ^. SubscriberDomain ==. val domain
        &&. tbl ^. SubscriberSubscriberType ==. val subType

create :: Subscriber -> SqlDB ()
create = Esq.create

deleteByKey :: (Text, Text) -> SqlDB ()
deleteByKey = Esq.deleteByKey @SubscriberT

findAll :: (MonadThrow m, Log m, Transactionable m) => m [Subscriber]
findAll =
  Esq.findAll $ do
    from $ table @SubscriberT
