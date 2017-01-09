{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Web server.

module Web.Server
       ( webServer
       ) where

import           Control.Monad.Except                 (MonadError (throwError))
import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.API                          ((:<|>) ((:<|>)), FromHttpApiData)
import           Servant.Server                       (Handler, ServantErr (errBody),
                                                       Server, ServerT, err404, serve)
import           Servant.Utils.Enter                  ((:~>) (Nat), enter)
import           Universum

import           Web.Api                              (AppApi, appApi)

-- | Run given `Application` on a given port inside a monad
serveImpl :: MonadIO m => m Application -> Word16 -> m ()
serveImpl application port =
    liftIO . run (fromIntegral port) . logStdoutDev =<< application

-- | Get the application
webApp :: MonadIO m => m Application
webApp = return $ serve appApi apiHandlers

-- | Run the whole thing
webServer :: MonadIO m => Word16 -> m ()
webServer = serveImpl webApp

-----------------------------------------------------------------------
-- Handlers
-----------------------------------------------------------------------

apiHandlers :: Server AppApi
apiHandlers = ping

ping :: Handler ()
ping = () <$ putText "Pong!"
