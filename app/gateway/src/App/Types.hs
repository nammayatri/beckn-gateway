{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App.Types where

import qualified Data.Cache as C
import qualified Data.Text as T
import EulerHS.Prelude
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
  registryLookup req = do
    registryUrl <- asks (.registryUrl)
    Registry.withSubscriberCache (Registry.registryLookup registryUrl) req

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Redis.get . ("gateway:registry:" <>) . lookupRequestToRedisKey
  setKey = Redis.set . ("gateway:registry:" <>) . lookupRequestToRedisKey
  delKey = Redis.del . ("gateway:registry:" <>) . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = (\k v -> Redis.setExp k v ttl.getSeconds) . ("gateway:registry:" <>) . lookupRequestToRedisKey
