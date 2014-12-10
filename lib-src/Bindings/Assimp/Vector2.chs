{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Vector2 where

import Control.Applicative
import Control.Monad
import Foreign.C.Types
import Foreign.Storable

#include <assimp/vector2.h>

{#context lib = "assimp" prefix = "ai" #}

data Vector2D = Vector2D { x'Vector3D :: Float
                         , y'Vector3D :: Float
                         } deriving (Read, Show)

instance Storable Vector2D where
  sizeOf _ = {#sizeof Vector2D #}
  alignment _ = {#alignof Vector2D #}
  peek p = Vector2D
    <$> liftM realToFrac ({#get Vector2D->x #} p)
    <*> liftM realToFrac ({#get Vector2D->y #} p)
  poke p (Vector2D x y) = do
    {#set Vector2D->x #} p (realToFrac x)
    {#set Vector2D->y #} p (realToFrac y)
