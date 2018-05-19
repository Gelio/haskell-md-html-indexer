{-|
Module      : Exception
Description : Gracefully handles exceptions in IO actions.

Gracefully handles exceptions in IO actions.
-}
module Exception
  ( handleIOError
  ) where

import           Control.Exception as X
import           System.IO.Error

-- |Handles any exceptions that were thrown during the execution of the action.
-- Displays a custom message when handling the exception.
handleIOError :: String -> IO () -> IO ()
handleIOError msg action =
  X.handle (anyErrorHandler msg) $ X.handle (ioErrorHandler msg) action

-- |Handles any exception displaying a custom message.
anyErrorHandler :: String -> X.SomeException -> IO ()
anyErrorHandler msg e = putStrLn $ msg ++ ": " ++ show e

-- |Handles IO exceptions displaying a custom message.
ioErrorHandler :: String -> X.IOException -> IO ()
ioErrorHandler msg e = putStrLn $ msg ++ ": " ++ getErrorMessage e


getErrorMessage :: X.IOException -> String
getErrorMessage e
  | isDoesNotExistError e = "File does not exist"
  | isPermissionError e = "Forbidden access"
  | otherwise = show e
