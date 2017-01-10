{-# LANGUAGE TemplateHaskell #-}

module Web.Types
       (
       ) where

import           Data.Aeson.TH (defaultOptions, deriveJSON)

import           Types         (AppState (..), KMeansParams (..), ProcessData (..))

deriveJSON defaultOptions ''KMeansParams
deriveJSON defaultOptions ''AppState
deriveJSON defaultOptions ''ProcessData
