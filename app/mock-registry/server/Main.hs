 {-
 This is the default license template.
 
 File: Main.hs
 Author: utkarshpandey
 Copyright (c) 2023 utkarshpandey
 
 To edit this license information: Press Ctrl+Shift+P and press 'Create new License Template...'.
-}

module Main where

import App (runRegistryService)
import Prelude

main :: IO ()
main = runRegistryService id
