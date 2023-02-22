 {-
 This is the default license template.
 
 File: Callback.hs
 Author: utkarshpandey
 Copyright (c) 2023 utkarshpandey
 
 To edit this license information: Press Ctrl+Shift+P and press 'Create new License Template...'.
-}

module Types.Beckn.API.Callback where

import Kernel.Types.Beckn.Error
import Data.Aeson
import EulerHS.Prelude hiding ((.=))
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
