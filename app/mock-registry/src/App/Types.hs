module App.Types
  ( Env,
    FlowHandler,
    FlowServer,
    Flow,
    AppCfg (),
    AppEnv (..),
    buildAppEnv,
    releaseAppEnv,
  )
where

import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics (CoreMetricsContainer, registerCoreMetricsContainer)
import Kernel.Types.App
import Kernel.Types.Common
import Kernel.Types.Flow
import Kernel.Utils.App (getPodName)
import Kernel.Utils.Dhall (FromDhall)
import Kernel.Utils.IOLogging
import Kernel.Utils.Shutdown
import EulerHS.Prelude

data AppCfg = AppCfg
  { port :: Int,
    esqDBCfg :: EsqDBConfig,
    graceTerminationPeriod :: Seconds,
    loggerConfig :: LoggerConfig,
    autoMigrate :: Bool,
    migrationPath :: Maybe FilePath
  }
  deriving (Generic, FromDhall)

data AppEnv = AppEnv
  { port :: Int,
    graceTerminationPeriod :: Seconds,
    loggerConfig :: LoggerConfig,
    isShuttingDown :: Shutdown,
    coreMetrics :: CoreMetricsContainer,
    loggerEnv :: LoggerEnv,
    esqDBEnv :: EsqDBEnv
  }
  deriving (Generic)

buildAppEnv :: AppCfg -> IO AppEnv
buildAppEnv AppCfg {..} = do
  isShuttingDown <- mkShutdown
  coreMetrics <- registerCoreMetricsContainer
  hostname <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  return AppEnv {..}

releaseAppEnv :: AppEnv -> IO ()
releaseAppEnv AppEnv {..} =
  releaseLoggerEnv loggerEnv

type Env = EnvR AppEnv

type FlowHandler = FlowHandlerR AppEnv

type FlowServer api = FlowServerR AppEnv api

type Flow = FlowR AppEnv
