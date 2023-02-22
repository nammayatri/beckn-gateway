 {-
 This is the default license template.
 
 File: Types.hs
 Author: utkarshpandey
 Copyright (c) 2023 utkarshpandey
 
 To edit this license information: Press Ctrl+Shift+P and press 'Create new License Template...'.
-}

module App.Types where

import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.App
import Kernel.Types.Cache
import Kernel.Types.Common hiding (id)
import Kernel.Types.Flow
import Kernel.Types.Registry
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import qualified Kernel.Utils.Registry as Registry
import Kernel.Utils.Servant.Client (HttpClientOptions, RetryCfg)
import Kernel.Utils.Servant.SignatureAuth
import qualified Data.Cache as C
import qualified Data.Text as T
import EulerHS.Prelude
import System.Environment (lookupEnv)
import Tools.Metrics

data AppCfg = AppCfg
  { hedisCfg :: Redis.HedisCfg,
    port :: Int,
    metricsPort :: Int,
    selfId :: Text,
    hostName :: Text,
    authEntity :: AuthenticatingEntity',
    loggerConfig :: LoggerConfig,
    graceTerminationPeriod :: Seconds,
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    registryUrl :: BaseUrl,
    disableSignatureAuth :: Bool
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { hedisEnv :: Redis.HedisEnv,
    hostName :: Text,
    authEntity :: AuthenticatingEntity',
    httpClientOptions :: HttpClientOptions,
    shortDurationRetryCfg :: RetryCfg,
    longDurationRetryCfg :: RetryCfg,
    registryUrl :: BaseUrl,
    disableSignatureAuth :: Bool,
    gwId :: Text,
    cache :: C.Cache Text Text,
    isShuttingDown :: TMVar (),
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv
  }
  deriving (Generic)

data CoreVersions = CoreVersions
  { mobility :: Text,
    logistics :: Text,
    localRetail :: Text,
    foodAndBeverage :: Text
  }
  deriving (Generic, FromDhall)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  hostname <- map T.pack <$> lookupEnv "POD_NAME"
  cache <- C.newCache Nothing
  isShuttingDown <- newEmptyTMVarIO
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  let modifierFunc = ("gateway:" <>)
  hedisEnv <- Redis.connectHedis hedisCfg modifierFunc
  return $
    AppEnv
      { gwId = selfId,
        ..
      }

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  Redis.disconnectHedis hedisEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.authEntity.signingKey)
  getSignatureExpiry = (.authEntity.signatureExpiry)

instance Registry Flow where
  registryLookup registryUrl = Registry.withSubscriberCache $ Registry.registryLookup registryUrl

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Redis.get . ("gateway:registry:" <>) . lookupRequestToRedisKey
  setKey = Redis.set . ("gateway:registry:" <>) . lookupRequestToRedisKey
  delKey = Redis.del . ("gateway:registry:" <>) . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = (\k v -> Redis.setExp k v ttl.getSeconds) . ("gateway:registry:" <>) . lookupRequestToRedisKey
