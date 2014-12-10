{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Camera where

import Control.Applicative
import Control.Monad
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

#include <assimp/camera.h>

{#context lib = "assimp" prefix = "ai" #}

{#import Bindings.Assimp.Types #}
{#import Bindings.Assimp.Vector3 #}

data Camera = Camera { name :: String
                     , position :: Vector3D
                     , up :: Vector3D
                     , lookAt :: Vector3D
                     , horizontalFOV :: Float
                     , clipPlaneNear :: Float
                     , clipPlaneFar :: Float
                     , aspect :: Float
                     } deriving (Read, Show)

instance Storable Camera where
  sizeOf _ = {#sizeof Camera #}
  alignment _ = {#alignof Camera #}
  peek p = do
    (AssimpString name) <- peekByteOff p {#offsetof Camera->mName #}
    Camera name
      <$> peekByteOff p {#offsetof Camera->mPosition #}
      <*> peekByteOff p {#offsetof Camera->mUp #}
      <*> peekByteOff p {#offsetof Camera->mLookAt #}
      <*> liftM realToFrac ({#get Camera->mHorizontalFOV #} p)
      <*> liftM realToFrac ({#get Camera->mClipPlaneNear #} p)
      <*> liftM realToFrac ({#get Camera->mClipPlaneFar #} p)
      <*> liftM realToFrac ({#get Camera->mAspect #} p)
  poke p c = do
    pokeByteOff p {#offsetof Camera->mName #} (AssimpString $ name c)
    pokeByteOff p {#offsetof Camera->mPosition #} (position c)
    pokeByteOff p {#offsetof Camera->mUp #} (up c)
    pokeByteOff p {#offsetof Camera->mLookAt #} (lookAt c)
    {#set Camera->mHorizontalFOV #} p (realToFrac $ horizontalFOV c)
    {#set Camera->mClipPlaneNear #} p (realToFrac $ clipPlaneNear c)
    {#set Camera->mClipPlaneFar #} p (realToFrac $ clipPlaneFar c)
    {#set Camera->mAspect #} p (realToFrac $ aspect c)
