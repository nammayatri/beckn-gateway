{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE LambdaCase #-}

module Flow.UpdateCities where

import App.Types (FlowHandler)
import qualified Data.Aeson as Aeson
import qualified Data.HashSet as Set
import Data.List ((\\))
import qualified Data.Text as T
import Domain.Types.UpdateCities
import EulerHS.Prelude
import Kernel.Storage.Esqueleto (runTransaction)
import Kernel.Types.Beckn.Country
import Kernel.Types.Common
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty (prettyShowViaJSON)
import qualified Storage.Queries.Subscriber as QSub
import Text.Regex (matchRegex, mkRegex)

notNull :: [a] -> Bool
notNull = not . null

guarded :: (a -> Bool) -> a -> Maybe a
guarded f a = if f a then Just a else Nothing

hasDuplicates :: Hashable a => [a] -> Bool
hasDuplicates = hasDuplicatesUsingSet Set.empty
  where
    hasDuplicatesUsingSet _ [] = False
    hasDuplicatesUsingSet s (x : xs) = x `Set.member` s || hasDuplicatesUsingSet (Set.insert x s) xs

-- TODO: Move above functions to shared-kernel

updateCities :: Maybe Text -> UpdateCitiesReq -> FlowHandler UpdateCitiesRes
updateCities _apiKey req = withFlowHandlerAPI' . withLogTag updateCitiesLogTag $ do
  internalApiKey <- asks (.internalAuthApiKey)
  when (Just internalApiKey /= _apiKey) . throwError $ InvalidRequest "Api key is invalid"
  logInfo $ "UpdateCitiesReq: " <> T.pack (prettyShowViaJSON req)
  subscribers <- QSub.findAllBy (Just req.uniqueKeyId) (Just req.subscriberId) (Just req.domain) (Just req.subscriberType) Nothing
  case subscribers of
    [sub] -> do
      (citiesAdded, citiesRemoved, newCities) <- getNewCities sub & fromEitherM InvalidRequest
      _ <- validateCities newCities sub.country & fromEitherM (InvalidRequest . (<>) "newCities: ")
      void . runTransaction $ QSub.updateCities req.uniqueKeyId req.subscriberId req.domain req.subscriberType newCities
      return $ UpdateCitiesRes {added = citiesAdded, removed = citiesRemoved}
    [] -> throwError $ InvalidRequest "No subscribers found for unique-key-id , might be a new Merchant"
    _ -> throwError $ InvalidRequest "Multiple subscribers returned for a unique key"
  where
    updateCitiesLogTag = "ukId:-" <> req.uniqueKeyId <> ",subId:-" <> req.subscriberId

    getNewCities sub = case (req.appendCities, req.replaceCities) of
      (Just appendCities, Nothing) -> do
        _ <- validateCities appendCities sub.country
        let newCities = sub.city <> appendCities
        Right (Just appendCities, Nothing, newCities)
      (Nothing, Just replaceCities) -> do
        _ <- validateCities replaceCities sub.country
        let citiesAdded = guarded notNull (replaceCities \\ sub.city)
            citiesRemoved = guarded notNull (sub.city \\ replaceCities)
        Right (citiesAdded, citiesRemoved, replaceCities)
      (Just _, Just _) -> Left "Both \"appendCities\" and \"replaceCities\" cannot be present in the request"
      (Nothing, Nothing) -> Left "Either \"appendCities\" or \"replaceCities\" should be present in the request"

validateCities :: [Text] -> Maybe Country -> Either Text ()
validateCities mbCities mbCountry =
  case mbCities of
    [] -> Left "Cities cannot be empty"
    ["*"] -> Right ()
    cities -> do
      let countryCode = maybe "std" countryCodePrefix mbCountry
      let patternStr = "^(std|" <> T.unpack countryCode <> "):[0-9]{2,8}$"
          invalidCities = filter (isNothing . matchRegex (mkRegex patternStr) . T.unpack) cities
      _ <- case invalidCities of
        [] -> Right ()
        xs -> Left $ "cities " <> T.pack (show xs) <> " doesn't match the regex pattern:" <> T.pack patternStr
      bool (Right ()) (Left "Cities cannot have duplicates") $ hasDuplicates cities

countryCodePrefix :: Country -> Text
countryCodePrefix India = "std"
countryCodePrefix country =
  case Aeson.toJSON country of
    Aeson.String code -> T.toLower code
    _ -> "std"

{-
 This check only allow same std / country codes cities in a merchant ,
 for example for India it will allow "std:***" but will not allow other country cities
 same for netherlands it will check with "nld:***"
-}
