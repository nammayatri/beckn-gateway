 {-
 This is the default license template.
 
 File: App.hs
 Author: utkarshpandey
 Copyright (c) 2023 utkarshpandey
 
 To edit this license information: Press Ctrl+Shift+P and press 'Create new License Template...'.
-}

module App
  ( runGateway,
  )
where

import App.Server
import App.Types
import qualified Kernel.Tools.Metrics.Init as Metrics
import qualified Kernel.Types.App as App
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.Servant.SignatureAuth
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import EulerHS.Prelude hiding (exitSuccess)
import EulerHS.Runtime as E
import qualified EulerHS.Runtime as R
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setPort,
  )
import System.Environment (lookupEnv)
import Tools.SignatureAuth (prepareAuthManagerWithRegistryUrl)
import Utils.Common

runGateway :: (AppCfg -> AppCfg) -> IO ()
runGateway configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "atlas-gateway"
  let port = appCfg.port
  let metricsPort = appCfg.metricsPort
  Metrics.serve metricsPort
  -- shutdown and activeConnections will be used to signal and detect our exit criteria
  appEnv <- buildAppEnv appCfg
  hostname <- (T.pack <$>) <$> lookupEnv "POD_NAME"
  let loggerRt = L.getEulerLoggerRuntime hostname $ appCfg.loggerConfig
      settings =
        defaultSettings
          & setGracefulShutdownTimeout (Just $ getSeconds appCfg.graceTerminationPeriod)
          & setInstallShutdownHandler (handleShutdown appEnv.isShuttingDown (releaseAppEnv appEnv))
          & setPort port
  E.withFlowRuntime (Just loggerRt) $ \flowRt -> do
    flowRt' <- runFlowR flowRt appEnv $ do
      withLogTag "Server startup" $ do
        managers <-
          prepareAuthManagerWithRegistryUrl
            flowRt
            appEnv
            ["Proxy-Authorization", "X-Gateway-Authorization"]
            appEnv.gwId
            appCfg.authEntity.uniqueKeyId
            & Map.singleton signatureAuthManagerKey
            & createManagers
        logInfo ("Runtime created. Starting server at port " <> show port)
        return $ flowRt {R._httpClientManagers = managers}
    runSettings settings $ run (App.EnvR flowRt' appEnv)
