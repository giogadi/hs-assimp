{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Anim where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.List
import qualified Data.Set as S
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

#include <assimp/anim.h>

{#context lib = "assimp" prefix = "ai" #}

{#import Bindings.Assimp.Types #}
{#import Bindings.Assimp.Quaternion #}
{#import Bindings.Assimp.Vector3 #}

data VectorKey = VectorKey { time'VectorKey :: Double
                           , value'VectorKey :: Vector3D
                           } deriving (Eq, Read, Show)

instance Storable VectorKey where
  sizeOf _ = {#sizeof VectorKey #}
  alignment _ = {#alignof VectorKey #}
  peek p = VectorKey
    <$> liftM realToFrac ({#get VectorKey->mTime #} p)
    <*> peekByteOff p {#offsetof VectorKey->mValue #}
  poke _ = undefined

data QuatKey = QuatKey { time'QuatKey :: Double
                       , value'QuatKey :: Quaternion
                       } deriving (Eq, Read, Show)

instance Storable QuatKey where
  sizeOf _ = {#sizeof QuatKey #}
  alignment _ = {#alignof QuatKey #}
  peek p = QuatKey
    <$> liftM realToFrac ({#get QuatKey->mTime #} p)
    <*> peekByteOff p {#offsetof QuatKey->mValue #}
  poke _ = undefined

{#enum AnimBehaviour {AnimBehaviour_DEFAULT as AnimBehaviourDefault,
                      AnimBehaviour_CONSTANT as AnimBehaviourConstant,
                      AnimBehaviour_LINEAR as AnimBehaviourLinear,
                      AnimBehaviour_REPEAT as AnimBehaviourRepeat,
                      _aiAnimBehaviour_Force32Bit as InvalidAnimBehaviour} deriving (Eq, Read, Show) #}

data NodeAnim = NodeAnim { name'NodeAnim :: String
                         , positionKeys'NodeAnim :: [VectorKey]
                         , rotationKeys'NodeAnim :: [QuatKey]
                         , scalingKeys'NodeAnim :: [VectorKey]
                         , preState'NodeAnim :: AnimBehaviour
                         , postState'NodeAnim :: AnimBehaviour
                         } deriving (Show)

instance Storable NodeAnim where
  sizeOf _ = {#sizeof NodeAnim #}
  alignment _ = {#alignof NodeAnim #}
  peek p = do
    AssimpString name <- peekByteOff p {#offsetof NodeAnim->mNodeName #}
    numPositionKeys <- fromIntegral <$> {#get NodeAnim->mNumPositionKeys #} p
    positionKeysPtr <- {#get NodeAnim->mPositionKeys #} p
    positionKeys <- peekArray numPositionKeys $ castPtr positionKeysPtr
    numRotationKeys <- fromIntegral <$> {#get NodeAnim->mNumRotationKeys #} p
    rotationKeysPtr <- {#get NodeAnim->mRotationKeys #} p
    rotationKeys <- peekArray numRotationKeys $ castPtr rotationKeysPtr
    numScalingKeys <- fromIntegral <$> {#get NodeAnim->mNumScalingKeys #} p
    scalingKeysPtr <- {#get NodeAnim->mScalingKeys #} p
    scalingKeys <- peekArray numScalingKeys $ castPtr scalingKeysPtr
    preState <- (toEnum . fromIntegral) <$> ({#get NodeAnim->mPreState #} p :: IO CInt)
    postState <- (toEnum . fromIntegral) <$> ({#get NodeAnim->mPostState #} p :: IO CInt)
    return $ NodeAnim name positionKeys rotationKeys scalingKeys preState postState
  poke _ = undefined

data Animation = Animation { name'Animation :: String
                           , duration'Animation :: Double
                           , ticksPerSecond'Animation :: Maybe Double
                           , channels'Animation :: [NodeAnim]
                           } deriving (Show)

instance Storable Animation where
  sizeOf _ = {#sizeof Animation #}
  alignment _ = {#alignof Animation #}
  peek p = do
    AssimpString name <- peekByteOff p {#offsetof Animation->mName #}
    duration <- realToFrac <$> ({#get Animation->mDuration #} p)
    tps <- realToFrac <$> ({#get Animation->mTicksPerSecond #} p)
    let ticksPerSecond = if tps == 0.0 then Nothing else Just tps
    numChannels <- fromIntegral <$> ({#get Animation->mNumChannels #} p)
    channelsPtr <- {#get Animation->mChannels #} p
    channelPtrs <- peekArray numChannels (castPtr channelsPtr)
    channels <- mapM peek channelPtrs
    return $ Animation name duration ticksPerSecond channels
  poke _ = undefined
