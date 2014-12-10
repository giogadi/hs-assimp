{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Vector3 where

import Control.Applicative
import Control.Monad
import Foreign.C.Types
import Foreign.Storable

#include <assimp/vector3.h>

{#context lib = "assimp" prefix = "ai" #}

data Vector3D = Vector3D { x'Vector3D :: Float
                         , y'Vector3D :: Float
                         , z'Vector3D :: Float
                         } deriving (Read, Show)

instance Storable Vector3D where
  sizeOf _ = {#sizeof Vector3D #}
  alignment _ = {#alignof Vector3D #}
  peek p = Vector3D
    <$> liftM realToFrac ({#get Vector3D->x #} p)
    <*> liftM realToFrac ({#get Vector3D->y #} p)
    <*> liftM realToFrac ({#get Vector3D->z #} p)
  poke p (Vector3D x y z) = do
    {#set Vector3D->x #} p (realToFrac x)
    {#set Vector3D->y #} p (realToFrac y)
    {#set Vector3D->z #} p (realToFrac z)
