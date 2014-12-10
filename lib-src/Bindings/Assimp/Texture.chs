{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Texture where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString as B
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

#include <assimp/texture.h>

{#context lib = "assimp" prefix = "ai" #}

{#import Bindings.Assimp.Types #}

data Texel = Texel { b'Texel :: Word8
                   , g'Texel :: Word8
                   , r'Texel :: Word8
                   , a'Texel :: Word8
                   } deriving (Show, Eq)

instance Storable Texel where
  sizeOf _ = {#sizeof Texel #}
  alignment _ = {#alignof Texel #}
  peek p = Texel
    <$> liftM fromIntegral ({#get Texel->b #} p)
    <*> liftM fromIntegral ({#get Texel->g #} p)
    <*> liftM fromIntegral ({#get Texel->r #} p)
    <*> liftM fromIntegral ({#get Texel->a #} p)
  poke p (Texel b g r a) = do
    {#set Texel->b #} p (fromIntegral b)
    {#set Texel->g #} p (fromIntegral g)
    {#set Texel->r #} p (fromIntegral r)
    {#set Texel->a #} p (fromIntegral a)

-- TODO use repa or something to make a proper multidim array of texels
data Texture = UncompressedTexture Int Int [Texel]
             | CompressedTexture B.ByteString deriving (Show)

instance Storable Texture where
  sizeOf _ = {#sizeof Texture #}
  alignment _ = {#alignof Texture #}
  peek p = do
    w <- fromIntegral <$> {#get Texture->mWidth #} p
    h <- fromIntegral <$> {#get Texture->mHeight #} p
    dataPtr <- {#get Texture->pcData #} p
    let isCompressed = h == 0
    if isCompressed
      then
        CompressedTexture <$> B.packCStringLen (castPtr dataPtr, w)
      else let numTexels = w * h
           in  UncompressedTexture w h
                 <$> peekArray numTexels (castPtr dataPtr)
  poke p = undefined
