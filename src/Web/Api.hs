{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Web API exposed by node.

module Web.Api
       ( AppApi
       , appApi
       ) where

import           Data.Proxy  (Proxy (Proxy))
import           Servant.API ((:<|>), (:>), Capture, Get, JSON, Post, QueryParam)
import           Universum

type AppApi =
    "ping" :> Get '[JSON] ()

appApi :: Proxy AppApi
appApi = Proxy
