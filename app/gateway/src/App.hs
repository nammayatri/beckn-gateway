{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module App
  ( runGateway,
  )
where

import App.Server
import App.Types
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text as T
import EulerHS.Prelude hiding (exitSuccess)
import EulerHS.Runtime as E
import qualified Kernel.Tools.Metrics.Init as Metrics
import qualified Kernel.Types.App as App
import Kernel.Types.Flow
import Kernel.Utils.App
import Kernel.Utils.Dhall (readDhallConfigDefault)
import qualified Kernel.Utils.FlowLogging as L
import Kernel.Utils.Servant.SignatureAuth
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setPort,
  )
import System.Environment (lookupEnv)
import Utils.Common

runGateway :: (AppCfg -> AppCfg) -> IO ()
runGateway configModifier = do
  appCfg <- configModifier <$> readDhallConfigDefault "beckn-gateway"
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
        let gatewayAuthManager = HMS.singleton (signatureAuthManagerKey) (prepareAuthManager flowRt appEnv ["Proxy-Authorization", "X-Gateway-Authorization"] appEnv.gwId appCfg.authEntity.uniqueKeyId)
        let becknAuthManager = HMS.singleton (signatureAuthManagerKey <> (T.pack "-") <> appEnv.gwId) (prepareAuthManager flowRt appEnv ["Authorization"] appEnv.gwId appCfg.authEntity.uniqueKeyId)
        flowRt' <- addAuthManagersToFlowRt flowRt [(Nothing, gatewayAuthManager), (Nothing, becknAuthManager)]
        logInfo ("Runtime created. Starting server at port " <> show port)
        return flowRt'
    runSettings settings $ run (App.EnvR flowRt' appEnv)
