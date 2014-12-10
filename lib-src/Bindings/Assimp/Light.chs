{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Light where

import Control.Applicative
import Control.Monad
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

#include <assimp/light.h>

{#context lib = "assimp" prefix = "ai" #}

{#import Bindings.Assimp.Types #}
{#import Bindings.Assimp.Vector3 #}

{#enum LightSourceType {LightSource_UNDEFINED as Undefined,
                        LightSource_DIRECTIONAL as Directional,
                        LightSource_SPOT as Spot,
                        _aiLightSource_Force32Bit as InvalidLightSource} deriving (Eq, Read, Show) #}

data Light = Light { name'Light :: String
                   , type'Light :: LightSourceType
                   , position'Light :: Vector3D
                   , direction'Light :: Vector3D
                   , attenuationConstant'Light :: Float
                   , attenuationLinear'Light :: Float
                   , attenuationQuadratic'Light :: Float
                   , colorDiffuse'Light :: Color3D
                   , colorSpecular'Light :: Color3D
                   , colorAmbient'Light :: Color3D
                   , angleInnerCone'Light :: Float
                   , angleOuterCone'Light :: Float
                   } deriving (Read, Show)

instance Storable Light where
  sizeOf _ = {#sizeof Light #}
  alignment _ = {#alignof Light #}
  peek p = do
    (AssimpString name) <- peekByteOff p {#offsetof Light->mName #}
    Light name
      <$> liftM (toEnum . fromIntegral) ({#get Light->mType #} p)
      <*> peekByteOff p {#offsetof Light->mPosition #}
      <*> peekByteOff p {#offsetof Light->mDirection #}
      <*> liftM realToFrac ({#get Light->mAttenuationConstant #} p)
      <*> liftM realToFrac ({#get Light->mAttenuationLinear #} p)
      <*> liftM realToFrac ({#get Light->mAttenuationQuadratic #} p)
      <*> peekByteOff p {#offsetof Light->mColorDiffuse #}
      <*> peekByteOff p {#offsetof Light->mColorSpecular #}
      <*> peekByteOff p {#offsetof Light->mColorAmbient #}
      <*> liftM realToFrac ({#get Light->mAngleInnerCone #} p)
      <*> liftM realToFrac ({#get Light->mAngleOuterCone #} p)
  poke p l = do
    pokeByteOff p {#offsetof Light->mName #} (AssimpString $ name'Light l)
    {#set Light->mType #} p (fromIntegral $ fromEnum $ type'Light l)
    pokeByteOff p {#offsetof Light->mPosition #} (position'Light l)
    pokeByteOff p {#offsetof Light->mDirection #} (direction'Light l)
    {#set Light->mAttenuationConstant #} p (realToFrac $ attenuationConstant'Light l)
    {#set Light->mAttenuationLinear #} p (realToFrac $ attenuationLinear'Light l)
    {#set Light->mAttenuationQuadratic #} p (realToFrac $ attenuationQuadratic'Light l)
    pokeByteOff p {#offsetof Light->mColorDiffuse #} (colorDiffuse'Light l)
    pokeByteOff p {#offsetof Light->mColorSpecular #} (colorSpecular'Light l)
    pokeByteOff p {#offsetof Light->mColorAmbient #} (colorAmbient'Light l)
    {#set Light->mAngleInnerCone #} p (realToFrac $ angleInnerCone'Light l)
    {#set Light->mAngleOuterCone #} p (realToFrac $ angleOuterCone'Light l)
