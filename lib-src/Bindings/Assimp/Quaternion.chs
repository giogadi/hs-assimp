{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Quaternion where

import Control.Applicative
import Control.Monad
import Foreign.C.Types
import Foreign.Storable

#include <assimp/quaternion.h>

{#context lib = "assimp" prefix = "ai" #}

data Quaternion = Quaternion { w :: Float
                             , x :: Float
                             , y :: Float
                             , z :: Float } deriving (Eq, Show, Read)

instance Storable Quaternion where
  sizeOf _ = {#sizeof Quaternion #}
  alignment _ = {#alignof Quaternion #}
  peek p = Quaternion
    <$> liftM realToFrac ({#get Quaternion->w #} p)
    <*> liftM realToFrac ({#get Quaternion->x #} p)
    <*> liftM realToFrac ({#get Quaternion->y #} p)
    <*> liftM realToFrac ({#get Quaternion->z #} p)
  poke _ = undefined
