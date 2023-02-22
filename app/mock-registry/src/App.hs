 {-
 This is the default license template.
 
 File: App.hs
 Author: utkarshpandey
 Copyright (c) 2023 utkarshpandey
 
 To edit this license information: Press Ctrl+Shift+P and press 'Create new License Template...'.
-}

module App where

import App.Routes (registryAPI, registryFlow)
import App.Types
import Kernel.Exit (exitDBMigrationFailure)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Migration
import Kernel.Utils.App
import Kernel.Utils.Dhall (readDhallConfigDefault)
import Kernel.Utils.Servant.Server (runServerWithHealthCheck)
import Servant (Context (..))

runRegistryService :: (AppCfg -> AppCfg) -> IO ()
runRegistryService configModifier = do
  config <- readDhallConfigDefault "mock-registry" <&> configModifier
  appEnv <- buildAppEnv config
  runServerWithHealthCheck appEnv registryAPI registryFlow middleware identity EmptyContext releaseAppEnv $ \flowRt -> do
    migrateIfNeeded config.migrationPath config.autoMigrate config.esqDBCfg
      >>= handleLeft exitDBMigrationFailure "Couldn't migrate database: "
    return flowRt
  where
    middleware = hashBodyForSignature
