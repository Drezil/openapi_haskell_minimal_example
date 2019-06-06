{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC
-fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module Example.API
  -- * Client and Server
  ( Config(..)
  , ExampleBackend(..)
  , createExampleClient
  , runExampleServer
  , runExampleClient
  , runExampleClientWithManager
  , callExample
  , ExampleClient
  , ExampleClientError(..)
  -- ** Servant
  , ExampleAPI
  ) where

import           Example.Types

import           Control.Monad.Catch                (Exception, MonadThrow, throwM)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader         (ReaderT (..))
import           Data.Aeson                         (Value)
import           Data.Coerce                        (coerce)
import           Data.Data                          (Data)
import           Data.Function                      ((&))
import qualified Data.Map                           as Map
import           Data.Monoid                        ((<>))
import           Data.Proxy                         (Proxy (..))
import           Data.Set                           (Set)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Data.UUID                          (UUID)
import           GHC.Exts                           (IsString (..))
import           GHC.Generics                       (Generic)
import           Network.HTTP.Client                (Manager, newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)
import           Network.HTTP.Types.Method          (methodOptions)
import qualified Network.Wai.Handler.Warp           as Warp
import           Servant                            (ServerError, serve)
import           Servant.API
import           Servant.API.Verbs                  (StdMethod (..), Verb)
import           Servant.Client                     (ClientEnv, Scheme (Http), ClientError, client,
                                                     mkClientEnv, parseBaseUrl)
import           Servant.Client.Core                (baseUrlPort, baseUrlHost)
import           Servant.Client.Internal.HttpClient (ClientM (..))
import           Servant.Server                     (Handler (..))
import           Web.FormUrlEncoded
import           Web.HttpApiData




-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList


-- | Servant type-level API, generated from the OpenAPI spec for Example.
type ExampleAPI
    =    "foo" :> ReqBody '[JSON] APIRequest :> Verb 'POST 200 '[JSON] APIResponse -- 'extractDescription' route


-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { configUrl :: String  -- ^ scheme://hostname:port/path, e.g. "http://localhost:8080/"
  } deriving (Eq, Ord, Show, Read)


-- | Custom exception type for our errors.
newtype ExampleClientError = ExampleClientError ClientError
  deriving (Show, Exception)
-- | Configuration, specifying the full url of the service.


-- | Backend for Example.
-- The backend can be used both for the client and the server. The client generated from the Example OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createExampleClient@). Alternatively, provided
-- a backend, the API can be served using @runExampleServer@.
data ExampleBackend m = ExampleBackend
  { extractDescription :: APIRequest -> m APIResponse{- ^  -}
  }

newtype ExampleClient a = ExampleClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  } deriving Functor

instance Applicative ExampleClient where
  pure x = ExampleClient (\_ -> pure x)
  (ExampleClient f) <*> (ExampleClient x) =
    ExampleClient (\env -> f env <*> x env)

instance Monad ExampleClient where
  (ExampleClient a) >>= f =
    ExampleClient (\env -> do
      value <- a env
      runClient (f value) env)

instance MonadIO ExampleClient where
  liftIO io = ExampleClient (\_ -> liftIO io)

createExampleClient :: ExampleBackend ExampleClient
createExampleClient = ExampleBackend{..}
  where
    ((coerce -> extractDescription)) = client (Proxy :: Proxy ExampleAPI)

-- | Run requests in the ExampleClient monad.
runExampleClient :: Config -> ExampleClient a -> ExceptT ClientError IO a
runExampleClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runExampleClientWithManager manager clientConfig cl

-- | Run requests in the ExampleClient monad using a custom manager.
runExampleClientWithManager :: Manager -> Config -> ExampleClient a -> ExceptT ClientError IO a
runExampleClientWithManager manager Config{..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a ExampleClientError
callExample
  :: (MonadIO m, MonadThrow m)
  => ClientEnv -> ExampleClient a -> m a
callExample env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err       -> throwM (ExampleClientError err)
    Right response -> pure response

-- | Run the Example server at the provided host and port.
runExampleServer
  :: (MonadIO m, MonadThrow m)
  => Config -> ExampleBackend (ExceptT ServerError IO) -> m ()
runExampleServer Config{..} backend = do
  url <- parseBaseUrl configUrl
  let warpSettings = Warp.defaultSettings
        & Warp.setPort (baseUrlPort url)
        & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ serve (Proxy :: Proxy ExampleAPI) (serverFromBackend backend)
  where
    serverFromBackend ExampleBackend{..} =
      (coerce extractDescription)
