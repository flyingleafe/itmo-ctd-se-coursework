{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Web API exposed by node.

module Web.Api
       ( AppApi
       , appApi
       ) where

import           Data.Proxy  (Proxy (Proxy))
import           Servant.API ((:<|>), (:>), Capture, Get, JSON, Post, QueryParam, ReqBody)
import           Universum

import           Config
import           Types       (ProcessData)

type AppApi =
    "initialize" :> ReqBody '[JSON] TMConfig :> Post '[JSON] ()
    :<|>
    "get_state" :> Get '[JSON] ProcessData
    :<|>
    "predict" :> ReqBody '[JSON] TMConfig :> Post '[JSON] [Double]

appApi :: Proxy AppApi
appApi = Proxy
