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
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import EulerHS.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types
import Kernel.Types.App
import Kernel.Types.Cache
import Kernel.Types.Common hiding (id)
import Kernel.Types.Flow
import Kernel.Types.Registry
import Kernel.Utils.App (lookupDeploymentVersion)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import qualified Kernel.Utils.Registry as Registry
import Kernel.Utils.Servant.Client (HttpClientOptions, RetryCfg)
import Kernel.Utils.Servant.SignatureAuth
import System.Environment (lookupEnv)
import Tools.Metrics

data AppCfg = AppCfg
  { hedisCfg :: Redis.HedisCfg,
    hedisClusterCfg :: Redis.HedisCfg,
    hedisNonCriticalCfg :: Redis.HedisCfg,
    hedisNonCriticalClusterCfg :: Redis.HedisCfg,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
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
    disableSignatureAuth :: Bool,
    enablePrometheusMetricLogging :: Bool,
    enableRedisLatencyLogging :: Bool,
    internalEndPointMap :: M.Map BaseUrl BaseUrl
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { hedisEnv :: Redis.HedisEnv,
    hedisClusterEnv :: Redis.HedisEnv,
    hedisNonCriticalEnv :: Redis.HedisEnv,
    hedisNonCriticalClusterEnv :: Redis.HedisEnv,
    hedisMigrationStage :: Bool,
    cutOffHedisCluster :: Bool,
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
    loggerEnv :: LoggerEnv,
    version :: DeploymentVersion,
    enablePrometheusMetricLogging :: Bool,
    enableRedisLatencyLogging :: Bool,
    internalEndPointHashMap :: HM.HashMap BaseUrl BaseUrl,
    requestId :: Maybe Text,
    shouldLogRequestId :: Bool,
    kafkaProducerForART :: Maybe KafkaProducerTools
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
  version <- lookupDeploymentVersion
  cache <- C.newCache Nothing
  isShuttingDown <- newEmptyTMVarIO
  coreMetrics <- registerCoreMetricsContainer
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  requestId <- pure Nothing
  shouldLogRequestId <- fromMaybe False . (>>= readMaybe) <$> lookupEnv "SHOULD_LOG_REQUEST_ID"
  let kafkaProducerForART = Nothing
  let modifierFunc = ("gateway:" <>)
  hedisEnv <- Redis.connectHedis hedisCfg modifierFunc
  hedisNonCriticalEnv <- Redis.connectHedis hedisNonCriticalCfg modifierFunc
  hedisNonCriticalClusterEnv <-
    if cutOffHedisCluster
      then pure hedisNonCriticalEnv
      else Redis.connectHedisCluster hedisNonCriticalClusterCfg modifierFunc
  hedisClusterEnv <-
    if cutOffHedisCluster
      then pure hedisEnv
      else Redis.connectHedisCluster hedisClusterCfg modifierFunc
  let internalEndPointHashMap = HM.fromList $ M.toList internalEndPointMap
  return $
    AppEnv
      { gwId = selfId,
        ..
      }

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} = do
  releaseLoggerEnv loggerEnv
  Redis.disconnectHedis hedisEnv
  Redis.disconnectHedis hedisClusterEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer r api = FlowServerR AppEnv api

type Flow = FlowR AppEnv

instance AuthenticatingEntity AppEnv where
  getSigningKey = (.authEntity.signingKey)
  getSignatureExpiry = (.authEntity.signatureExpiry)

instance Registry Flow where
  registryLookup req = do
    registryUrl <- asks (.registryUrl)
    selfId <- asks (.gwId)
    Registry.withSubscriberCache (\req' -> Registry.registryLookup registryUrl req' selfId) req

instance Cache Subscriber Flow where
  type CacheKey Subscriber = SimpleLookupRequest
  getKey = Redis.get . ("gateway:registry:" <>) . lookupRequestToRedisKey
  setKey = Redis.set . ("gateway:registry:" <>) . lookupRequestToRedisKey
  delKey = Redis.del . ("gateway:registry:" <>) . lookupRequestToRedisKey

instance CacheEx Subscriber Flow where
  setKeyEx ttl = (\k v -> Redis.setExp k v ttl.getSeconds) . ("gateway:registry:" <>) . lookupRequestToRedisKey
