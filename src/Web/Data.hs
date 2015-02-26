module Data where

import Data.Typeable (TypeRep)
import Data.Proxy (Proxy)

type Underlying = String

data ContractGUIRepr  =
    GUIRepr   { guiLabel     :: String
               , params      :: [(String, TypeRep)]
               , url         :: String
               , guiReprType :: TypeRep }
