{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Types.Beckn.API.Callback where

import Data.Aeson
import EulerHS.Prelude
import Kernel.Types.Beckn.Error
import Types.Beckn.Context

-- Creating own gateway CallbackReq to support Context for 0.8 and 0.9

data CallbackReq a = CallbackReq
  { context :: Context,
    contents :: Either Error a
  }
  deriving (Generic, Show)

instance ToJSON a => ToJSON (CallbackReq a) where
  toJSON (CallbackReq context contents) = object allFields
    where
      contextField = "context" .= context
      allFields = case contents of
        Left err -> contextField : ["error" .= err]
        Right message -> contextField : ["message" .= message]

instance FromJSON a => FromJSON (CallbackReq a) where
  parseJSON = withObject "CallbackReq" $ \o ->
    CallbackReq
      <$> o .: "context"
      <*> (Left <$> o .: "error" <|> Right <$> o .: "message")
