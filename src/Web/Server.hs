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
import           Control.Exception                    (ioError)
import qualified Control.Monad.Catch                  as Catch
import           Control.Monad.Except                 (MonadError (throwError))
import           Control.Monad.State                  (get)
import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant.API                          ((:<|>) ((:<|>)), FromHttpApiData)
import           Servant.Server                       (Handler, ServantErr (errBody),
                                                       Server, ServerT, err404, serve)
import           Servant.Utils.Enter                  ((:~>) (Nat), enter)
import           System.IO.Error                      (userError)
import           Universum

import           Config                               (TMConfig)
import           Types                                (Base, ProcessData, TMError,
                                                       TVarStateT (..), WorkMode,
                                                       runBaseRaw)

import           Web.Api                              (AppApi, appApi)
import           Web.Types                            ()

-- | Run given `Application` on a given port inside a monad
serveImpl :: MonadIO m => m Application -> Word16 -> m ()
serveImpl application port =
    liftIO . run (fromIntegral port) . logStdoutDev =<< application

-- | Get the application
webApp :: WorkMode m => m (m :~> Handler) -> m Application
webApp nat = flip enter apiHandlers <$> nat >>= return . serve appApi

-- | Run the whole thing
webServer :: Word16 -> Base ()
webServer = serveImpl $ webApp nat

-----------------------------------------------------------------------
-- Handler transformation
-----------------------------------------------------------------------

toIOError :: IO (Either TMError a) -> IO a
toIOError action =
    action >>= either (ioError . userError . toString) return

convertHandler
    :: forall a. TVar ProcessData
    -> Base a
    -> Handler a
convertHandler tv handler = do
    liftIO (toIOError . runBaseRaw tv $ handler)
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

apiHandlers :: WorkMode m => ServerT AppApi m
apiHandlers = initialize :<|> get

initialize :: WorkMode m => TMConfig -> m ()
initialize _ = pure ()
