{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Web server.

module Web.Server
       ( webServer
       ) where

import           Control.Concurrent.STM               (TVar)
import           Control.Lens                         (use, (.=))
import qualified Control.Monad.Catch                  as Catch
import           Control.Monad.Except                 (MonadError (throwError))
import           Control.Monad.State                  (get)
import           Formatting                           (int, sformat, stext, (%))
import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.API                          ((:<|>) ((:<|>)), FromHttpApiData)
import           Servant.Server                       (Handler, ServantErr (errBody),
                                                       Server, ServerT, err404, err500,
                                                       serve)
import           Servant.Utils.Enter                  ((:~>) (Nat), enter)
import           Universum

import           Config                               (TMConfig (..))
import           Runner                               (fork_, pipeline)
import           Types
import           Web.Api                              (AppApi, appApi)
import           Web.Types                            ()

-- | Run given `Application` on a given port inside a monad
serveImpl :: MonadIO m => m Application -> Word16 -> m ()
serveImpl application port =
    liftIO . run (fromIntegral port) . logStdoutDev =<< application

-- | Get the application
webApp :: Base (Base :~> Handler) -> Base Application
webApp nat = serve appApi . flip enter apiHandlers <$> nat

-- | Run the whole thing
webServer :: Word16 -> Base ()
webServer port = do
    putText $ sformat ("Starting server on port "%int) port
    serveImpl (webApp nat) port

-----------------------------------------------------------------------
-- Handler transformation
-----------------------------------------------------------------------

setBody :: ServantErr -> Text -> ServantErr
setBody err txt = err { errBody = encodeUtf8 txt }

toServantHandler :: IO (Either TMError a) -> Handler a
toServantHandler action = lift action >>= either throwErr return
  where throwErr = throwError . setBody err500

convertHandler
    :: forall a. TVar ProcessData
    -> Base a
    -> Handler a
convertHandler tv handler =
    toServantHandler (runBaseRaw tv handler)
    `Catch.catches`
    excHandlers
  where
    excHandlers = [Catch.Handler catchServant]
    catchServant = throwError

nat :: Base (Base :~> Handler)
nat = do
    tv <- TVarStateT ask
    return $ Nat (convertHandler tv)

-----------------------------------------------------------------------
-- Handlers
-----------------------------------------------------------------------

apiHandlers :: ServerT AppApi Base
apiHandlers = initialize :<|> get

initialize :: TMConfig -> Base ()
initialize TMConfig{..} = do
    as <- use appState
    when (as /= Await) $
        throwError "Server state is not `Await` now"
    appState .= Processing
    -- | Start working thread
    fork_ $ pipeline tmDocFilePath

-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------
