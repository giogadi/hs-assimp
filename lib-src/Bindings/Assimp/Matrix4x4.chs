{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Matrix4x4 where

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

#include <assimp/matrix4x4.h>

{#context lib = "assimp" prefix = "ai" #}

data Matrix4x4 = Matrix4x4 { a1 :: Float
                           , a2 :: Float
                           , a3 :: Float
                           , a4 :: Float
                           , b1 :: Float
                           , b2 :: Float
                           , b3 :: Float
                           , b4 :: Float
                           , c1 :: Float
                           , c2 :: Float
                           , c3 :: Float
                           , c4 :: Float
                           , d1 :: Float
                           , d2 :: Float
                           , d3 :: Float
                           , d4 :: Float
                           } deriving (Eq, Show, Read)

instance Storable Matrix4x4 where
  sizeOf _ = {#sizeof Matrix4x4 #}
  alignment _ = {#alignof Matrix4x4 #}
  peek p = Matrix4x4
    <$> liftM realToFrac ({#get Matrix4x4->a1 #} p)
    <*> liftM realToFrac ({#get Matrix4x4->a2 #} p)
    <*> liftM realToFrac ({#get Matrix4x4->a3 #} p)
    <*> liftM realToFrac ({#get Matrix4x4->a4 #} p)

    <*> liftM realToFrac ({#get Matrix4x4->b1 #} p)
    <*> liftM realToFrac ({#get Matrix4x4->b2 #} p)
    <*> liftM realToFrac ({#get Matrix4x4->b3 #} p)
    <*> liftM realToFrac ({#get Matrix4x4->b4 #} p)

    <*> liftM realToFrac ({#get Matrix4x4->c1 #} p)
    <*> liftM realToFrac ({#get Matrix4x4->c2 #} p)
    <*> liftM realToFrac ({#get Matrix4x4->c3 #} p)
    <*> liftM realToFrac ({#get Matrix4x4->c4 #} p)

    <*> liftM realToFrac ({#get Matrix4x4->d1 #} p)
    <*> liftM realToFrac ({#get Matrix4x4->d2 #} p)
    <*> liftM realToFrac ({#get Matrix4x4->d3 #} p)
    <*> liftM realToFrac ({#get Matrix4x4->d4 #} p)
  poke p m = do
    {#set Matrix4x4->a1 #} p (realToFrac $ a1 m)
    {#set Matrix4x4->a2 #} p (realToFrac $ a2 m)
    {#set Matrix4x4->a3 #} p (realToFrac $ a3 m)
    {#set Matrix4x4->a4 #} p (realToFrac $ a4 m)

    {#set Matrix4x4->b1 #} p (realToFrac $ b1 m)
    {#set Matrix4x4->b2 #} p (realToFrac $ b2 m)
    {#set Matrix4x4->b3 #} p (realToFrac $ b3 m)
    {#set Matrix4x4->b4 #} p (realToFrac $ b4 m)

    {#set Matrix4x4->c1 #} p (realToFrac $ c1 m)
    {#set Matrix4x4->c2 #} p (realToFrac $ c2 m)
    {#set Matrix4x4->c3 #} p (realToFrac $ c3 m)
    {#set Matrix4x4->c4 #} p (realToFrac $ c4 m)

    {#set Matrix4x4->d1 #} p (realToFrac $ d1 m)
    {#set Matrix4x4->d2 #} p (realToFrac $ d2 m)
    {#set Matrix4x4->d3 #} p (realToFrac $ d3 m)
    {#set Matrix4x4->d4 #} p (realToFrac $ d4 m)
