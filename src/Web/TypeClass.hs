{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification, StandaloneDeriving  #-}

module TypeClass where

import Data.Data
import Data.Proxy
import GHC.Generics
import CodeGen.DataGen
import PersistentData

{-
class CGUIRep a where
    toForm :: Proxy (Rep a) -> [(String, TypeRep)]
    default toForm :: (Generic a, GUIRep a) => Proxy a -> [(String, TypeRep)]
    toForm = gtoForm
-}

data FormField b = forall a. (Field a b, Show a) => MkField (Proxy a)

deriving instance (Show (FormField b))

pack :: (Field a b, Show a) => Proxy a -> FormField b
pack = MkField

-- TODO: use proper type instead of String for default value
-- (should be Maybe a, but it doesn't work well with existential)
class Field a b where
    fieldHtml :: Proxy a -> Maybe String -> String -> b

class GUIRep f b where
    gtoForm :: Proxy f  -> [(String, FormField b)]

instance (GUIRep a b, GUIRep a1 b) => GUIRep (a :*: a1) b where
    gtoForm _ = gtoForm (Proxy :: Proxy a) ++ gtoForm (Proxy :: Proxy a1)

instance (Selector s, Field t b, Show t) => GUIRep (M1 S s (K1 R t)) b where
    gtoForm _ = [(selName (undefined :: M1 S s (K1 R t) ()), pack (Proxy :: Proxy t))]

instance (GUIRep f b) => GUIRep (M1 D x f) b where
    gtoForm _ = gtoForm (Proxy :: Proxy f)

instance (GUIRep f b) => GUIRep (M1 C x f) b where
    gtoForm _ = gtoForm (Proxy :: Proxy f)
