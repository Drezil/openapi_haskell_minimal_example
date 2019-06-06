{-# LANGUAGE RecordWildCards #-}

import Example.API as API

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (ClientEnv, mkClientEnv, parseBaseUrl)
import Data.Text as T


main :: IO ()
main = do
  -- Configure the BaseUrl for the client
  url <- parseBaseUrl "http://localhost:8080/"

  -- You probably want to reuse the Manager across calls, for performance reasons
  manager <- newManager tlsManagerSettings

  -- Create the client (all endpoint functions will be available)
  let ExampleBackend{..} = API.createExampleClient

  -- Any Example API call can go here, e.g. here we call `getSomeEndpoint`
  result <- API.callExample (mkClientEnv manager url) (extractDescription (T.pack "foo"))
  print result
