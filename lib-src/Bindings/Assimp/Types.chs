{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Types where

import Control.Applicative
import Control.Monad
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

#include <assimp/types.h>
#include <assimp/color4.h>

{#context lib = "assimp" prefix = "ai" #}

{#import Bindings.Assimp.Vector3 #}

--------------------------------------------------------------------------------

data Plane = Plane { a'Plane :: Float
                   , b'Plane :: Float
                   , c'Plane :: Float
                   , d'Plane :: Float
                   } deriving (Read, Show)

instance Storable Plane where
  sizeOf _ = {#sizeof Plane #}
  alignment _ = {#alignof Plane #}
  peek p = Plane
    <$> liftM realToFrac ({#get Plane->a #} p)
    <*> liftM realToFrac ({#get Plane->b #} p)
    <*> liftM realToFrac ({#get Plane->c #} p)
    <*> liftM realToFrac ({#get Plane->d #} p)
  poke p (Plane a b c d) = do
    {#set Plane->a #} p (realToFrac a)
    {#set Plane->b #} p (realToFrac b)
    {#set Plane->c #} p (realToFrac c)
    {#set Plane->d #} p (realToFrac d)

--------------------------------------------------------------------------------

data Ray = Ray { pos'Ray :: Vector3D
               , dir'Ray :: Vector3D
               } deriving (Read, Show)

instance Storable Ray where
  sizeOf _ = {#sizeof Ray #}
  alignment _ = {#alignof Ray #}
  peek p = Ray
    <$> peekByteOff p {#offsetof Ray->pos #}
    <*> peekByteOff p {#offsetof Ray->dir #}
  poke p (Ray pos dir) = do
    pokeByteOff p {#offsetof Ray->pos #} pos
    pokeByteOff p {#offsetof Ray->dir #} dir

--------------------------------------------------------------------------------

data Color3D = Color3D { r'Color3D :: Float
                       , g'Color3D :: Float
                       , b'Color3D :: Float
                       } deriving (Read, Show)

instance Storable Color3D where
  sizeOf _ = {#sizeof Color3D #}
  alignment _ = {#alignof Color3D #}
  peek p = Color3D
    <$> liftM realToFrac ({#get Color3D->r #} p)
    <*> liftM realToFrac ({#get Color3D->g #} p)
    <*> liftM realToFrac ({#get Color3D->b #} p)
  poke p (Color3D r g b) = do
    {#set Color3D->r #} p (realToFrac r)
    {#set Color3D->g #} p (realToFrac g)
    {#set Color3D->b #} p (realToFrac b)

--------------------------------------------------------------------------------

data Color4D = Color4D { r'Color4D :: Float
                       , g'Color4D :: Float
                       , b'Color4D :: Float
                       , a'Color4D :: Float
                       } deriving (Read, Show)

instance Storable Color4D where
  sizeOf _ = {#sizeof Color4D #}
  alignment _ = {#alignof Color4D #}
  peek p = Color4D
    <$> liftM realToFrac ({#get Color4D->r #} p)
    <*> liftM realToFrac ({#get Color4D->g #} p)
    <*> liftM realToFrac ({#get Color4D->b #} p)
    <*> liftM realToFrac ({#get Color4D->a #} p)
  poke p (Color4D r g b a) = do
    {#set Color4D->r #} p (realToFrac r)
    {#set Color4D->g #} p (realToFrac g)
    {#set Color4D->b #} p (realToFrac b)
    {#set Color4D->a #} p (realToFrac a)

--------------------------------------------------------------------------------

newtype AssimpString = AssimpString String deriving (Eq, Ord, Read, Show)

instance Storable AssimpString where
  sizeOf _ = {#sizeof String #}
  alignment _ = {#alignof String #}
  peek p = do
    print "String 1"
    len <- {#get String->length #} p
    print "String 2"
    let strStartPtr = plusPtr p {#offsetof String->data #}
    print "String 3"
    print $ "len: " ++ show len
    cstr <- peekArray (fromIntegral len) strStartPtr
    print "String 4"
    return $ AssimpString $ map castCCharToChar cstr
  poke p (AssimpString str) = do
    {#set String->length #} p (fromIntegral $ length str)
    let strStartPtr = plusPtr p {#offsetof String->data #}
    pokeArray strStartPtr (str ++ "\0")

--------------------------------------------------------------------------------

{#enum Return as ReturnType {Return_SUCCESS as Success,
                             Return_FAILURE as Failure,
                             Return_OUTOFMEMORY as OutOfMemory,
                             _AI_ENFORCE_ENUM_SIZE as NumReturnTypes} deriving (Read, Show, Eq) #}

{#enum Origin {Origin_SET as OriginSet,
               Origin_CUR as OriginCurrent,
               Origin_END as OriginEnd,
               _AI_ORIGIN_ENFORCE_ENUM_SIZE as NumOrigins} deriving (Read, Show, Eq) #}

{#enum DefaultLogStream {DefaultLogStream_FILE as LogFile,
                         DefaultLogStream_STDOUT as LogStdOut,
                         DefaultLogStream_STDERR as LogStdErr,
                         DefaultLogStream_DEBUGGER as LogDebugger,
                         _AI_DLS_ENFORCE_ENUM_SIZE as NumDefaultLogStreams} deriving (Read, Show, Eq) #}

--------------------------------------------------------------------------------

data MemoryInfo = MemoryInfo { textures'MemoryInfo :: Int
                             , materials'MemoryInfo :: Int
                             , meshes'MemoryInfo :: Int
                             , nodes'MemoryInfo :: Int
                             , animations'MemoryInfo :: Int
                             , cameras'MemoryInfo :: Int
                             , lights'MemoryInfo :: Int
                             , total'MemoryInfo :: Int
                             } deriving (Read, Show)

instance Storable MemoryInfo where
  sizeOf _ = {#sizeof MemoryInfo #}
  alignment _ = {#alignof MemoryInfo #}
  peek p = MemoryInfo
    <$> liftM fromIntegral ({#get MemoryInfo->textures #} p)
    <*> liftM fromIntegral ({#get MemoryInfo->materials #} p)
    <*> liftM fromIntegral ({#get MemoryInfo->meshes #} p)
    <*> liftM fromIntegral ({#get MemoryInfo->nodes #} p)
    <*> liftM fromIntegral ({#get MemoryInfo->animations #} p)
    <*> liftM fromIntegral ({#get MemoryInfo->cameras #} p)
    <*> liftM fromIntegral ({#get MemoryInfo->lights #} p)
    <*> liftM fromIntegral ({#get MemoryInfo->total #} p)
  poke p (MemoryInfo tex mat mesh n a c l tot) = do
    {#set MemoryInfo->textures #} p (fromIntegral tex)
    {#set MemoryInfo->materials #} p (fromIntegral mat)
    {#set MemoryInfo->meshes #} p (fromIntegral mesh)
    {#set MemoryInfo->nodes #} p (fromIntegral n)
    {#set MemoryInfo->animations #} p (fromIntegral a)
    {#set MemoryInfo->cameras #} p (fromIntegral c)
    {#set MemoryInfo->lights #} p (fromIntegral l)
    {#set MemoryInfo->total #} p (fromIntegral tot)
