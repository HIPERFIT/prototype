module Data where

import Data.Typeable (TypeRep)
import TypeClass

type Underlying = String

data ContractGUIRepr  =
    GUIRepr { guiLabel :: String
            , params   :: [(String, TypeRep)]
            , url      :: String }
