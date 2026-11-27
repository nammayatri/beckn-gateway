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

-- | Ordered oldest-created-first for a stable pick among duplicate rows.
findAllBy' :: (MonadThrow m, Log m, Transactionable m) => Maybe Text -> Maybe Text -> Maybe Context.Domain -> Maybe SubscriberType -> m [Subscriber]
findAllBy' mbKeyId mbSubId mbDomain mbSubType =
  Esq.findAll $ do
    subscriber <- from $ table @SubscriberT
    where_ $
      whenJust_ mbKeyId (\keyId -> subscriber ^. SubscriberUniqueKeyId ==. val keyId)
        &&. whenJust_ mbSubId (\subId -> subscriber ^. SubscriberSubscriberId ==. val subId)
        &&. whenJust_ mbDomain (\domain -> subscriber ^. SubscriberDomain ==. val domain)
        &&. whenJust_ mbSubType (\subType -> subscriber ^. SubscriberSubscriberType ==. val subType)
    orderBy [asc (subscriber ^. SubscriberCreated)]
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

-- | No unique constraint to target with an upsert, so look up first.
create :: Subscriber -> SqlDB ()
create sub = do
  existing <- findAllBy' (Just sub.unique_key_id) (Just sub.subscriber_id) (Just sub.domain) (Just sub._type)
  case existing of
    (old : _) -> updateSubscriber old sub
    [] -> Esq.create sub

-- | Writes only changed fields; never touches city (use updateCities) or
-- nulls a field from an incoming Nothing.
updateSubscriber :: Subscriber -> Subscriber -> SqlDB ()
updateSubscriber old new =
  case setClauses of
    [] -> pure ()
    clauses ->
      Esq.update $ \tbl -> do
        set tbl (clauses <> [SubscriberUpdated =. val new.updated])
        where_ $
          tbl ^. SubscriberUniqueKeyId ==. val new.unique_key_id
            &&. tbl ^. SubscriberSubscriberId ==. val new.subscriber_id
            &&. tbl ^. SubscriberDomain ==. val new.domain
            &&. tbl ^. SubscriberSubscriberType ==. val new._type
  where
    changedWhen cond clause = if cond then Just clause else Nothing
    changedToJust newVal cond clause = if isJust newVal && cond then Just clause else Nothing
    setClauses =
      catMaybes
        [ changedWhen (old.subscriber_url /= new.subscriber_url) (SubscriberSubscriberUrl =. val (showBaseUrl new.subscriber_url)),
          changedToJust new.country (old.country /= new.country) (SubscriberCountry =. val new.country),
          changedWhen (old.signing_public_key /= new.signing_public_key) (SubscriberSigningPublicKey =. val new.signing_public_key),
          changedToJust new.encr_public_key (old.encr_public_key /= new.encr_public_key) (SubscriberEncrPublicKey =. val new.encr_public_key),
          changedToJust new.valid_from (old.valid_from /= new.valid_from) (SubscriberValidFrom =. val new.valid_from),
          changedToJust new.valid_until (old.valid_until /= new.valid_until) (SubscriberValidUntil =. val new.valid_until),
          changedToJust new.status (show @Text old.status /= show @Text new.status) (SubscriberStatus =. val new.status)
        ]

deleteByKey :: (Text, Text) -> SqlDB ()
deleteByKey = Esq.deleteByKey @SubscriberT

findAll :: (MonadThrow m, Log m, Transactionable m) => m [Subscriber]
findAll =
  Esq.findAll $ do
    from $ table @SubscriberT
