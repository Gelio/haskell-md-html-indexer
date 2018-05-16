module Exception (handleIOError) where

import Control.Exception as X
import System.IO.Error

handleIOError :: String -> IO () -> IO ()
handleIOError msg action = X.handle (anyErrorHandler msg) $ X.handle (ioErrorHandler msg) action

anyErrorHandler :: String -> X.SomeException -> IO ()
anyErrorHandler msg e = putStrLn $ msg ++ ": " ++ show e

ioErrorHandler :: String -> X.IOException -> IO ()
ioErrorHandler msg e = putStrLn $ msg ++ ": " ++ getErrorMessage e

getErrorMessage :: X.IOException -> String
getErrorMessage e
  | isDoesNotExistError e = "File does not exist"
  | isPermissionError e = "Forbidden access"
  | otherwise = show e
