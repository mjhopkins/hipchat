{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
{-# OPTIONS_GHC -fno-warn-unused-matches     #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing     #-}

import           HipChat.Util                         (ToFromJSON)

import qualified Blaze.ByteString.Builder             as B

import           Control.Category                     hiding (id, (.))
import           Control.Concurrent                   (forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
-- import           Control.Concurrent.Lifted
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Control          (MonadBaseControl)

import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base64               as Base64 (encode)
import qualified Data.ByteString.Lazy.Char8           as LBS
import           Data.Default
import           Data.Foldable
import           Data.IORef
import           Data.List
import           Data.Machine.MealyT                  (MealyT (MealyT),
                                                       runMealyT)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           Data.Maybe
import           Data.Monoid                          ((<>))
import           Data.Proxy
import           Data.String
import           Data.String.Conversions              (cs)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.IO                         as T
import           Data.Time
import           Data.Time.Calendar
import           Data.Tuple                           (swap)
import           Util.JsonRequestLogger               (formatAsJSON)

import           Debug.Trace

import           GHC.Generics
import           GHC.IO.Unsafe

import           Network.HTTP.Client                  as HTTP (Manager, defaultManagerSettings,
                                                               managerModifyRequest,
                                                               managerModifyResponse,
                                                               managerSetProxy,
                                                               newManager,
                                                               proxyEnvironment,
                                                               responseBody,
                                                               responseStatus)
import           Network.HTTP.Client.TLS              (tlsManagerSettings)
import           Network.HTTP.Media                   ((//), (/:))
import           Network.Wai
import           Network.Wai.Handler.Warp             (Port)
import qualified Network.Wai.Handler.Warp             as Warp
import           Network.Wai.Middleware.RequestLogger

import           Prelude                              hiding ((/))

import           Servant
import           Servant.API
import           Servant.Client
import           Servant.Common.Req
import           Servant.Utils.Enter

import           System.IO
import           System.Log.FastLogger

--------------------------------------------------------------------------------
-- ChatInteraction
--------------------------------------------------------------------------------

type ChatInteraction = MealyT IO (User, Msg) Msg

type User = Text
type Msg  = Text

--------------------------------------------------------------------------------
-- Conf, AppError, App monad
--------------------------------------------------------------------------------

data Conf = Conf
  { -- confRooms           :: IORef (Map RoomId (IORef (Registration, Maybe TokenData)))
    confChatInteraction :: MVar ChatInteraction
  }

makeLensesWith camelCaseFields ''Conf

data AppError = AppError -- todo
  -- = RoomNotRegistered
  -- | MissingTokenURL
  -- | SErr ServantError
  -- | InvalidUrl String
  deriving (Eq, Show)

type App = ReaderT Conf (ExceptT AppError IO)

appToServant :: Conf -> App :~> Servant.Handler
appToServant conf = NT $ (>>= either handleErr return) . liftIO . runExceptT . flip runReaderT conf
   where
     handleErr e = let err = "App integration error: " <> LBS.pack (show e)
                   in throwError $ err500 { errBody = err }
                   --TODO format error better

--------------------------------------------------------------------------------
-- Simple server
--
-- Basic Servant chat server with no HipChat integration
-- For testing
--------------------------------------------------------------------------------

data SimpleMsg = SimpleMsg { simpleMsgFrom :: User, simpleMsgContent :: Text }
  deriving (Eq, Show, Generic)

instance ToFromJSON SimpleMsg

type SimpleAPI =
  "message" :> ReqBody '[JSON] SimpleMsg :> Post '[JSON] Text

handleSimpleMsg :: (MonadReader Conf m, MonadIO m) => SimpleMsg -> m Text
handleSimpleMsg m = do
  liftIO . print $ m
  chatMVar <- view chatInteraction
  let
    user = simpleMsgFrom m
    msg  = simpleMsgContent m
  resp <- liftIO . modifyMVar chatMVar $ \chat -> swap <$> runMealyT chat (user, msg)
  return resp

simpleServerImpl :: ServerT SimpleAPI App
simpleServerImpl = handleSimpleMsg

simpleServer :: ChatInteraction -> Conf -> Server SimpleAPI
simpleServer chat conf = enter (appToServant conf) simpleServerImpl

simpleServerApp :: ChatInteraction -> Conf -> Application
simpleServerApp chat = serve (Proxy :: Proxy SimpleAPI) . simpleServer chat

runSimpleServer :: ChatInteraction -> Port -> IO ()
runSimpleServer chat port = do
  let mw = id
  -- mw <- requestLogger
  putStrLn "creating new conf"
  conf <- Conf <$> newMVar chat
  Warp.run port . mw $ simpleServerApp chat conf

main :: IO ()
main = do
  putStrLn $ "Running on port " ++ show p
  runSimpleServer echoChat p
    where
      p = 8126

{-

curl -iv -XPOST localhost:8126/message \
-H 'Content-type: application/json' \
-d "{\"from\":\"Mark\",\"content\":\"hi\"}"

-}

echoChat :: ChatInteraction
echoChat = go 0 where
  go :: Int -> ChatInteraction
  go n = MealyT $ \(user, msg) -> do
    putStrLn $ T.unpack $ "(" <> T.pack (show n) <> ") " <> user <> ": " <> msg
    return . ("You said, \"" <> msg <> "\".",) $ go (n+1)
