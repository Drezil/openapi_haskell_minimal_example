{-# LANGUAGE RecordWildCards #-}

import Example.API
import Example.Types
import Data.Text
import Control.Monad.Catch
import Control.Monad.IO.Class

-- Run a Example server on localhost:8080
main :: IO ()
main = do
  let server = ExampleBackend impl
      config = Config "http://localhost:8080/"
  runExampleServer config server

impl :: (MonadIO m, MonadThrow m) => APIRequest -> m APIResponse
impl (APIRequest x y) = return $ APIResponse [x] [y]
