{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports     #-}
{-# OPTIONS_GHC -fno-warn-unused-matches     #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing     #-}

module Main where

import           HipChat.AddOn.Capabilities
import           HipChat.AddOn.Registration           (Registration (..),
                                                       capabilitiesUrl)
import qualified HipChat.AddOn.Registration           as Registration
import           HipChat.AddOn.Types
import           HipChat.AddOn.Webhooks               hiding (id)
import qualified HipChat.AddOn.Webhooks               as Webhooks
import qualified HipChat.API.Capabilities             as API
import qualified HipChat.API.Rooms                    as API
import qualified HipChat.API.Types                    as API
import           HipChat.Auth                         hiding (RoomNotification)
import qualified HipChat.Auth                         as Auth
import           HipChat.Auth.Types                   (TokenResp (..),
                                                       accessToken, expiresIn,
                                                       scope)
import qualified HipChat.Auth.Types                   as Auth
import           HipChat.Types

import qualified Blaze.ByteString.Builder             as B

import           Control.Category                     hiding (id, (.))
import           Control.Concurrent                   (forkIO)
import           Control.Concurrent.Async
import           Control.Concurrent.Lifted
import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base64               as Base64 (encode)
import           Data.Default
import           Data.Foldable
import           Data.IORef
import           Data.List
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
-- TokenData
--------------------------------------------------------------------------------

data TokenData = TokenData
  { tokenDataExpiry      :: UTCTime
  , tokenDataAccessToken :: Text
  , tokenDataApiScopes   :: [APIScope]
  } deriving (Eq, Show)

-- makeLensesWith camelCaseFields ''TokenData

instance Auth.HasAccessToken TokenData Text where
  accessToken f (TokenData e t s) =
    (\t' -> TokenData e t' s) <$> f t

class HasApiScopes s a | s -> a where
  apiScopes :: Lens' s a

--TODO merge with Auth.HasScopes
instance HasApiScopes TokenData [APIScope] where
  apiScopes f (TokenData e t s) =
     TokenData e t <$> f s

class HasExpiry s a | s -> a where
  expiry :: Lens' s a

instance HasExpiry TokenData UTCTime where
  expiry f (TokenData e t s) =
    (\e' -> TokenData e' t s) <$> f e


mkTokenData :: TokenResp -> UTCTime -> TokenData
mkTokenData tok issueTime =
  TokenData
  { tokenDataExpiry      = expirySeconds `addUTCTime` issueTime
  , tokenDataAccessToken = tok ^. accessToken
  , tokenDataApiScopes   = tok ^. scope
  }
  where
    expirySeconds = fromIntegral $ tok ^. expiresIn - leeway
    leeway = 5

tokenStillValid :: MonadIO m => TokenData -> m Bool
tokenStillValid tok = do
  now <- liftIO getCurrentTime
  return $ tok ^. expiry < now

--------------------------------------------------------------------------------
-- Conf, AppError, HipChat monad
--------------------------------------------------------------------------------

newtype Conf = Conf
  { confRooms :: IORef (Map RoomId (IORef (Registration, Maybe TokenData)))
  }

makeLensesWith camelCaseFields ''Conf

data AppError
  = RoomNotRegistered
  | MissingTokenURL
  | SErr ServantError
  | InvalidUrl String
  deriving (Eq, Show)

type HipChat = ReaderT Conf (ExceptT AppError IO)

hipChatToServant :: IO Conf -> HipChat :~> ExceptT ServantErr IO
hipChatToServant conf = Nat $ \hipchat -> do
  c <- liftIO conf
  lift . fmap (either (error . show) id) . runExceptT . flip runReaderT c $ hipchat
  --TODO better error handling

--------------------------------------------------------------------------------
-- API definitions
--------------------------------------------------------------------------------

type HipChatAddOnApi =
       Get '[JSON] AddOn
  :<|> "capabilities"      :> Get '[JSON] AddOn
  :<|> "installed"         :> ReqBody '[JSON] Registration :> Post '[JSON] ()
  :<|> "room_archived"     :> ReqBody '[JSON] Value :> Post '[JSON] ()
  :<|> "room_created"      :> ReqBody '[JSON] Value :> Post '[JSON] ()
  :<|> "room_deleted"      :> ReqBody '[JSON] Value :> Post '[JSON] ()
  :<|> "room_enter"        :> ReqBody '[JSON] Value :> Post '[JSON] ()
  :<|> "room_exit"         :> ReqBody '[JSON] Value :> Post '[JSON] ()
  :<|> "room_file_upload"  :> ReqBody '[JSON] Value :> Post '[JSON] ()
  :<|> "room_message"      :> ReqBody '[JSON] Value :> Post '[JSON] ()
  :<|> "room_notification" :> ReqBody '[JSON] RoomNotificationResp :> Post '[JSON] ()
  :<|> "room_topic_change" :> ReqBody '[JSON] Value :> Post '[JSON] ()
  :<|> "room_unarchived"   :> ReqBody '[JSON] Value :> Post '[JSON] ()

type FullApi = HipChatAddOnApi :<|> "static" :> Raw

--------------------------------------------------------------------------------
-- Server implementation
--------------------------------------------------------------------------------

hipChatServer :: URL -> ServerT HipChatAddOnApi HipChat
hipChatServer baseUrl =
       capabilitiesDescriptor baseUrl -- TODO redirect
  :<|> capabilitiesDescriptor baseUrl
  :<|> handleInstallation
  :<|> handleRoomMessage "room_archived"
  :<|> handleRoomMessage "room_created"
  :<|> handleRoomMessage "room_deleted"
  :<|> handleRoomMessage "room_enter"
  :<|> handleRoomMessage "room_exit"
  :<|> handleRoomMessage "room_file_upload"
  :<|> handleRoomMessage "room_message"
  :<|> handleRoomNotification
  :<|> handleRoomMessage "room_topic_change"
  :<|> handleRoomMessage "room_unarchived"

handleInstallation :: (MonadIO m, MonadError AppError m, MonadReader Conf m) => Registration -> m ()
handleInstallation reg = do
  liftIO . putStrLn $ "Received an installation"
  liftIO . putStrLn $ "  oauthId: "          <> show (reg ^. Registration.oauthId)
  liftIO . putStrLn $ "  capabilities URL: " <> show (reg ^. Registration.capabilitiesUrl)
  liftIO . putStrLn $ "  room id: "          <> show (reg ^. Registration.roomId)
  liftIO . putStrLn $ "  group id: "         <> show (reg ^. Registration.groupId)
  liftIO . putStrLn $ "  OAuth secret: "     <> show (reg ^. Registration.oauthSecret)

  conf <- ask
  case reg ^. Registration.roomId of
    Just r -> do
      ref <- liftIO $ newIORef (reg, Nothing)
      liftIO $ modifyIORef' (conf ^. rooms) (Map.insert r ref)

      --TODO fix
      --TODO error won't work with async without linking. `Debug.Trace` for now

      --TODO tidy up: run monad, then convert to IO.
      let action = do
            putStrLn "Getting manager"
            man <- getManager
            putStrLn "About to get room token"
            tok <- fmap (either (\e -> trace (show e) (error (show e))) id) . runExceptT . flip runReaderT conf $ getRoomToken man r
            putStrLn $ "Token is" <> show tok
            putStrLn "About to post to room"
            fmap (either (\e -> trace (show e) (error (show e))) id) . runExceptT $ postMessageToRoom r tok man "https://api.hipchat.com/" -- TODO get from somewhere else
            putStrLn "Posted to room"
            return tok
      void . liftIO $ forkIO (void action)

  --     liftIO $ putStrLn "Getting room token..."
  --     man <- getManager
  --     tok <- getRoomToken man id
  --     let at = tok ^. Auth.accessToken
  --     liftIO . putStrLn $ "Access token is: " <> cs at
  --     liftIO $ putStrLn "Testing token by trying to post to room"
  --
  --     return ()
    Nothing -> return () --TODO handleOtherInstallation
  return ()

postMessageToRoom :: (MonadError AppError m, MonadIO m) => RoomId -> TokenData -> Manager -> URL -> m ()
postMessageToRoom r tok man baseUri = do
  let at = tok ^. accessToken
  liftIO . putStrLn $ "Access token is: " <> cs at
  liftIO . putStrLn $ "Posting a message to room " <> show r
  timestampId <- runClient man baseUri $ API.sendPlainTextRoomNotification (Token at) (RoomNum r) "Hello from Haskell"
  liftIO $ print timestampId
  return ()
--
-- getRoomToken' :: (MonadReader Conf m, MonadIO m, MonadError AppError m) => RoomId -> m Token
-- getRoomToken' r = getManager >>= \man -> getRoomToken man r

--TODO scopes. Hardcoded to [SendNotification] for now
getRoomToken :: (MonadReader Conf m, MonadIO m, MonadError AppError m) => Manager -> RoomId -> m TokenData
getRoomToken manager room = do
  -- get conf, see if room has an entry
  -- if no entry, can't. Signal failure
  -- if there's a token and it's still valid (within allowance), use
  -- else, use url, id and secret to make oauth post
  -- get back response with token
  -- save to conf under room
  liftIO . putStrLn $ "Getting room token for room " <> show room
  conf <- ask
  roomMap <- liftIO . readIORef $ conf ^. rooms
  ref <- case Map.lookup room roomMap of
       Just r  -> return r
       Nothing -> throwError RoomNotRegistered
  (reg, td) <- liftIO $ readIORef ref
  let u = cs $ reg ^. Registration.oauthId
  let p = cs $ reg ^. Registration.oauthSecret
  liftIO . putStrLn $ "OAuth id is " ++ show u
  liftIO . putStrLn $ "OAuth secret is " ++ show p
  let scopes = [SendNotification, SendMessage, ViewMessages, ViewRoom]
  tok' <- case td of
    Just tok -> do
        stillValid <- tokenStillValid tok
        liftIO . putStrLn $ "token still valid: " ++ show stillValid
        let hasScopes = all (`elem` (tok ^. Main.apiScopes)) scopes
        liftIO . putStrLn $ "token has right scopes: " ++ show hasScopes
        liftIO . putStrLn $ "needed: " ++ show scopes ++ ", has " ++ show (tok ^. Main.apiScopes)
        if stillValid && hasScopes
        then return tok
        else getNewToken manager room (reg ^. capabilitiesUrl) u p scopes
    Nothing          -> getNewToken manager room (reg ^. capabilitiesUrl) u p scopes
    --TODO refactor
  liftIO $ writeIORef ref (reg, Just tok')
  return tok'

type OAuthAPI =  ReqBody '[FormUrlEncoded, JSON]  Auth.TokenReq
  :> BasicAuth "oauth2" ()
  :> Post '[JSON] Auth.TokenResp

oauthClient :: Auth.TokenReq -> BasicAuthData -> ClientM TokenResp
oauthClient = client (Proxy :: Proxy OAuthAPI)
--
-- -- runClientM :: ClientM a -> ClientEnv -> IO (Either ServantError a)
-- runClientM'' :: ClientM a -> ClientEnv -> ExceptT ServantError IO a
-- runClientM'' cm env = (`runReaderT` env) $ runClientM' cm
--


--
-- getManager :: IO Manager
getManager :: MonadIO m => m Manager
getManager =
  let settings = managerSetProxy (proxyEnvironment Nothing) tlsManagerSettings
        { managerModifyRequest = \req -> do
          putStrLn "REQUEST"
          print req
          return req
        -- , managerModifyResponse = \resp -> do
        --   putStrLn "RESPONSE"
        --   let status = HTTP.responseStatus resp
        --   print status
        --   r <- HTTP.responseBody resp
        --   putStrLn $ cs r
        --   return resp
        }
  in liftIO $ newManager settings

runClientIO :: Manager -> URL -> ClientM a -> IO a
runClientIO man url client = do
  res <- runClientM client $ ClientEnv man (toBaseUrl url)
  either (fail . show) return res

runClient :: (MonadError AppError m, MonadIO m) => Manager -> URL -> ClientM a -> m a
runClient man url client = do
  res <- liftIO $ runClientM client $ ClientEnv man (toBaseUrl url)
  case res of
    Left err -> throwError (SErr err) -- TODO handle better. Retries?
    Right r  -> return r
    --     either handleError (\r -> prettyPrint r >> putStrLn "Success" >> return r) res

-- installation post-back contains oauth uname & pw + capabilities doc url
-- using the capabilities document url from the installation from the server,
-- retrieve capabilities document from the server
-- get out capabilities.oauth2Provider.tokenUrl
-- post to tokenUrl with basic auth and oauth credentials
getNewToken :: forall m. (MonadIO m, MonadError AppError m)
            => Manager -> RoomId -> URL -> ByteString -> ByteString -> [APIScope] -> m TokenData
getNewToken manager r capabilitiesUrl uname pw scopes = do
  liftIO . putStrLn $ "Token URL is:"
  tokenUrl <- getTokenUrl
  liftIO . print $ tokenUrl
  liftIO . putStrLn $ "Requesting new OAuth token using " ++ show uname ++ ", " ++ show pw
  tok <- doAuthReq tokenUrl scopes
  liftIO . putStrLn $ "Token is " ++ show tok
  now <- liftIO getCurrentTime
  return $ mkTokenData tok now
  where
    getTokenUrl :: m URL
    getTokenUrl = do

      cap <- runClient manager capabilitiesUrl API.getCapabilities
      let url = cap ^. API.capabilities ^? _Just . API.oauth2Provider . _Just . tokenUrl . _Just
      liftIO . putStrLn $ "Token url is: " ++ show url
      maybe (throwError MissingTokenURL) return url

    doAuthReq :: (MonadError AppError m, MonadIO m) => URL -> [APIScope] -> m TokenResp
    doAuthReq uri scopes =
      let
        req = (Auth.tokenReq Auth.ClientCredentials) { Auth.tokenReqScope = scopes }
        authData = BasicAuthData uname pw
      in runClient manager uri $ oauthClient req authData


handleRoomNotification :: MonadIO m => RoomNotificationResp -> m ()
handleRoomNotification n = liftIO $ print n

handleRoomMessage :: MonadIO m => String -> Value -> m ()
handleRoomMessage name s = do
  liftIO $ putStrLn name
  liftIO $ print s

capabilitiesDescriptor :: Monad m => URL -> m AddOn
capabilitiesDescriptor baseUrl = return AddOn
  { addOnKey          = "test-haskell-add-on"
  , addOnName         = "test Haskell add-on for HipChat"
  , addOnDescription  = "first attempt at writing a HipChat add-on in Haskell"
  , addOnLinks        = mkLinks $ baseUrl / "capabilities"
  , addOnCapabilities = Just Capabilities
    { capabilitiesInstallable =
        Just Installable
          { installableCallbackUrl       = Just $ baseUrl / "installed"
          , installableInstalledUrl      = Nothing
          , installableUninstalledUrl    = Nothing
          , installableUpdateCallbackUrl = Just $ baseUrl / "installed"
          , installableUpdatedUrl        = Nothing
          , installableAllowRoom         = True
          , installableAllowGlobal       = False
          }
    , capabilitiesHipchatApiConsumer =
        Just APIConsumer  -- Maybe APIConsumer
          { apiConsumerScopes =
            [ SendNotification
            , SendMessage
            , ViewMessages
            , ViewGroup
            , ViewMessages
            , ViewRoom
            ]
          , apiConsumerFromName = Just "test Haskell addon"
          , apiConsumerAvatar   = Just $ baseUrl / "static" / "icon.png" -- Maybe URL
          }
    , capabilitiesOauth2Provider     = Nothing
    , capabilitiesWebhook            =
        [ Webhook (baseUrl / "room_archived"    ) RoomArchived     (Just "/hey") (Just "Room archived webhook") (Just "Room archived webhook") Nothing
        , Webhook (baseUrl / "room_created"     ) RoomCreated      (Just "/hey") (Just "Room created webhook") (Just "Room created webhook") Nothing
        , Webhook (baseUrl / "room_deleted"     ) RoomDeleted      (Just "/hey") (Just "Room deleted webhook") (Just "Room deleted webhook") Nothing
        , Webhook (baseUrl / "room_enter"       ) RoomEnter        (Just "/hey") (Just "Room enter webhook") (Just "Room enter webhook") Nothing
        , Webhook (baseUrl / "room_exit"        ) RoomExit         (Just "/hey") (Just "Room exit webhook") (Just "Room exit webhook") Nothing
        , Webhook (baseUrl / "room_file_upload" ) RoomFileUpload   (Just "/hey") (Just "Room file upload webhook") (Just "Room file upload webhook") Nothing
        , Webhook (baseUrl / "room_message"     ) RoomMessage      (Just "/hey") (Just "Room message webhook") (Just "Room message webhook") Nothing
        , Webhook (baseUrl / "room_notification") RoomNotification (Just "/hey") (Just "Room notification webhook") (Just "Room notification webhook") Nothing
        , Webhook (baseUrl / "room_topic_change") RoomTopicChange  (Just "/hey") (Just "Room topic change webhook") (Just "Room topic change webhook") Nothing
        , Webhook (baseUrl / "room_unarchived"  ) RoomUnarchived   (Just "/hey") (Just "Room unarchivedwebhook") (Just "Room unarchivedwebhook") Nothing
        ]
    , capabilitiesConfigurable       = Nothing
    , capabilitiesDialog             = []
    , capabilitiesWebPanel           = []
    , capabilitiesGlance             = []
    , capabilitiesAdminPage          = Nothing
    }
  , addOnVendor       = Just (Vendor baseUrl "Mark")
  , addOnApiVersion   = Nothing
 }

mainServer :: URL -> Server FullApi
mainServer baseUrl = enter (hipChatToServant conf) (hipChatServer baseUrl) :<|> serveDirectory "public-www"
 where
    conf :: IO Conf
    conf = Conf <$> newIORef Map.empty

app :: URL -> Application
app baseUrl = serve (Proxy :: Proxy FullApi) (mainServer baseUrl)

requestLogger :: IO Middleware
requestLogger = mkRequestLogger $
  def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

run :: URL -> Port -> IO ()
run baseUrl localPort = do
  mw <- requestLogger
  Warp.run localPort $ mw (app baseUrl)

main :: IO ()
main = do
  mode <- hGetBuffering stdout
  putStrLn $ "IO buffering is " ++ show mode
  run "https://9cb9983c.ngrok.io" 8126
-- main = run "https://hipchat-proxy-container.dev.bedrock.marathon.ragnarok.services.sop.ai.cba:8126" 8126
