 {-
 This is the default license template.
 
 File: Main.hs
 Author: utkarshpandey
 Copyright (c) 2023 utkarshpandey
 
 To edit this license information: Press Ctrl+Shift+P and press 'Create new License Template...'.
-}

module Main where

import App (runGateway)
import EulerHS.Prelude

main :: IO ()
main = runGateway id
