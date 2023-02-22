 {-
 This is the default license template.
 
 File: Context.hs
 Author: utkarshpandey
 Copyright (c) 2023 utkarshpandey
 
 To edit this license information: Press Ctrl+Shift+P and press 'Create new License Template...'.
-}

module Types.Beckn.Context where

import Data.Aeson
import Data.Text
import EulerHS.Prelude hiding ((.=))
import Servant.Client (BaseUrl)
import Types.Beckn.Domain

data Context = Context
  { domain :: Domain,
    bap_uri :: BaseUrl,
    bpp_uri :: Maybe BaseUrl,
    transaction_id :: Maybe Text
  }
  deriving (Generic, FromJSON, Show)

instance ToJSON Context where
  toJSON = genericToJSON $ defaultOptions {omitNothingFields = True}
