{-# LANGUAGE TemplateHaskell #-}

module Web.Types
       (
       ) where

import           Data.Aeson.TH (defaultOptions, deriveJSON)

import           Types         (AppState (..), ProcessData (..))

deriveJSON defaultOptions ''AppState
deriveJSON defaultOptions ''ProcessData
