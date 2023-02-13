let LogLevel = < DEBUG | INFO | WARNING | ERROR >

let loggerConfig =
      { level = LogLevel.DEBUG
      , logToFile = False
      , logToConsole = True
      , logRawSql = False
      , prettyPrinting = False
      }

let httpClientOptions = { timeoutMs = +2000 }

let shortDurationRetryCfg = { maxRetries = +3, baseCoefficient = +2 }

let longDurationRetryCfg = { maxRetries = +3, baseCoefficient = +4 }

in  { autoMigrate = False
    , loggerConfig
    , LogLevel
    , signatureExpiry = +600
    , httpClientOptions
    , shortDurationRetryCfg
    , longDurationRetryCfg
    }
