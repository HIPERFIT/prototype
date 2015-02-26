{-# LANGUAGE OverloadedStrings, GADTs #-}
module Main where

import CallOption
import qualified RainbowOption as RO
import Contract hiding (i)
import Pricing
import Utils
import DataProviders.Csv
import DataProviders.Common
import DataProviders.Data
import Data
import View
import Service
import Types
import CodeGen.DataGen

import Data.Time
import Web.Scotty hiding (body, params)
import Network.Wai.Middleware.Static
import CSS
import Data.Aeson (object, (.=), FromJSON(..), decode, eitherDecode, Value (..))
import Data.Text.Lazy (toStrict)
import Data.Monoid (mconcat, mempty)
import Control.Monad.Trans
import Control.Monad (forM_)
import Control.Applicative ((<$>), (<*>))
import qualified Data.Text as T
import GHC.Generics (Rep, Generic)
import Data.Proxy
import Data.Data (typeOf, Typeable)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BL

instance FromJSON OptionData
instance FromJSON RO.RainbowOption

callOption = GUIRepr { guiLabel = "Call option"
                     , params = gtoForm (Proxy :: Proxy (Rep OptionData))
                     , url = "callOption"
                     , guiReprType = typeOf (undefined :: OptionData) }


rainbowOption = GUIRepr { guiLabel = "Rainbow option"
                        , params = gtoForm (Proxy :: Proxy (Rep RO.RainbowOption))
                        , url = "rainbowOption"
                        , guiReprType = typeOf (undefined :: RO.RainbowOption) }

allContracts = [callOption, rainbowOption]

data Inputable where
    Inp :: (PricerInput a) => a -> Inputable

pack :: (PricerInput a) => a -> Inputable
pack = Inp

toInput (Inp v) = makeInput v

inp = do
  ty <- param "type"
  d <- case (ty :: String) of
         "callOption"  -> do x <- jsonData :: ActionM OptionData
                             return $ pack x
         "rainbowOption" -> do x <- jsonData :: ActionM RO.RainbowOption
                               return $ pack x  
  res <- liftIO $ toInput d
  return res

toMap = M.fromList . map (\c -> (url c, c))

urlMap = toMap allContracts

main = scotty 3000 $ do
    get "/" $ homeView allContracts
    get (capture $ contractsBaseUrl ++ ":type") $ do
                       ty <- param "type"
                       contractView allContracts (urlMap M.! ty)
    defaultService inp availableUnderlyings getStoredQuotes pConf
    
