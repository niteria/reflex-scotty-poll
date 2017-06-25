{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Auth
  ( checkPassword
  , protectedResources
  ) where

import Network.Wai (Request, pathInfo)
import Data.SecureMem -- for constant-time comparison
import Data.ByteString (ByteString)

password :: SecureMem
password = secureMemFromByteString "correct horse battery staple"

checkPassword :: Monad m => ByteString -> ByteString -> m Bool
checkPassword u p =
  return $ u == "admin" && secureMemFromByteString p == password

protectedResources ::  Request -> IO Bool
protectedResources request = do
  let path = pathInfo request
  return $ protect path
  where
    protect (p : _) =  p == "admin"
      -- all requests to /admin/* should be authenticated
    protect _       =  False
      -- other requests are allowed for anonymous users
