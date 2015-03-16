{-# LANGUAGE OverloadedStrings #-}
module Auth where

import Web.Scotty as W
import Network.HTTP.Types.Status
import Network.Wai
import Control.Applicative((<$>))
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Lazy as TL
import Database.Persist
import DB
import PersistentData
import Control.Monad.Trans

data AuthResult = Authorized | Unauthorized deriving (Show)

_AUTH_ENABLED = True

toBool :: AuthResult -> Bool
toBool Authorized = True
toBool Unauthorized = False

askBasicAuthentication = do
  W.status status401
  addHeader "WWW-Authenticate" "Basic realm=\"please-auth\""
  html $ "unauthorized"

authResult test b64AuthVal = either (\_ -> Unauthorized) (\(l,p) -> test l (B.drop 1 p)) authVal
    where authVal = (B64.decode $ B.drop 6 b64AuthVal) >>= (return . B.break (== columnSep))
              where columnSep = B.head ":"

parseUserAndPass val = (B64.decode $ B.drop 6 val) >>= (return . f . B.break (== (B.head ":")))  
    where
      f (u,p) = (u, B.drop 1 p)

basicAuth granted | _AUTH_ENABLED = 
                      do
                        hs <- requestHeaders <$> request
                        case lookup "Authorization" hs of
                          Nothing -> askBasicAuthentication
                          (Just auth) -> case (parseUserAndPass auth) of
                                           (Left err) -> html $ TL.pack err
                                           (Right (usr, pass)) -> do
                                                         auth <- liftIO $ checkDbAuth usr pass
                                                         case auth of
                                                           Authorized -> granted
                                                           Unauthorized -> askBasicAuthentication
                  | otherwise = granted

checkDbAuth usr pass = do
  res <- runDb $ selectList [UserUsername ==. (BC.unpack usr), UserPassword ==. (BC.unpack pass)] []
  return $ if (null res) then Unauthorized else Authorized

