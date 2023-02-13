{-# LANGUAGE TemplateHaskell #-}

module Types.Error
  ( module Types.Error,
    module Kernel.Types.Error,
  )
where

import Kernel.Types.Error
import Kernel.Types.Error.BaseError.HTTPError
import EulerHS.Prelude

data GatewayError
  = NoProviders
  deriving (Eq, Show, IsAPIError)

instanceExceptionWithParent 'HTTPException ''GatewayError

instance IsBaseError GatewayError

instance IsHTTPError GatewayError where
  toErrorCode NoProviders = "CORE001"

instance IsBecknAPIError GatewayError where
  toType NoProviders = DOMAIN_ERROR
