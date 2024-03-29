# Auto-Generated OpenAPI Bindings to `Example`

The library in `lib` provides auto-generated-from-OpenAPI bindings to the Example API.

## Installation

Installation follows the standard approach to installing Stack-based projects.

1. Install the [Haskell `stack` tool](http://docs.haskellstack.org/en/stable/README).
2. Run `stack install` to install this package.

Otherwise, if you already have a Stack project, you can include this package under the `packages` key in your `stack.yaml`:
```yaml
packages:
- location:
    git: https://github.com/yourGitOrg/yourGitRepo
    commit: somecommit
```

## Main Interface

The main interface to this library is in the `Example.API` module, which exports the ExampleBackend type. The ExampleBackend
type can be used to create and define servers and clients for the API.

## Creating a Client

A client can be created via the `createExampleClient` function, which will generate a function for every endpoint of the API.
Then these functions can be invoked with `runExampleClientWithManager` or more conveniently with `callExampleClient`
(depending if you want an `Either` back or you want to catch) to access the API endpoint they refer to, if the API is served
at the `url` you specified.

For example, if `localhost:8080` is serving the Example API, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import Example.API as API

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (ClientEnv, mkClientEnv, parseBaseUrl)


main :: IO ()
main = do
  -- Configure the BaseUrl for the client
  url <- parseBaseUrl "http://localhost:8080/"

  -- You probably want to reuse the Manager across calls, for performance reasons
  manager <- newManager tlsManagerSettings

  -- Create the client (all endpoint functions will be available)
  ExampleBackend{..} <- API.createExampleClient

  -- Any Example API call can go here, e.g. here we call `getSomeEndpoint`
  API.callExample (mkClientEnv manager url) getSomeEndpoint
```

## Creating a Server

In order to create a server, you must use the `runExampleServer` function. However, you unlike the client, in which case you *got* a `ExampleBackend`
from the library, you must instead *provide* a `ExampleBackend`. For example, if you have defined handler functions for all the
functions in `Example.Handlers`, you can write:

```haskell
{-# LANGUAGE RecordWildCards #-}

import Example.API

-- A module you wrote yourself, containing all handlers needed for the ExampleBackend type.
import Example.Handlers

-- Run a Example server on localhost:8080
main :: IO ()
main = do
  let server = ExampleBackend{..}
      config = Config "http://localhost:8080/"
  runExampleServer config server
```
