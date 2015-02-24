{-# LANGUAGE KindSignatures #-}
module Data where

import Data.Typeable (TypeRep)
import Data.Proxy (Proxy)

type Underlying = String

data ContractGUIRepr (a :: * -> *) = GUIRepr { guiLabel   :: String
                                 , params     :: Proxy a
                                 , url        :: String }
