{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Scene where

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

#include <assimp/scene.h>
#include <assimp/cimport.h>

{#context lib = "assimp" prefix = "ai" #}

{#import Bindings.Assimp.Types #}
{#import Bindings.Assimp.Texture #}
{#import Bindings.Assimp.Mesh #}
{#import Bindings.Assimp.Light #}
{#import Bindings.Assimp.Camera #}
{#import Bindings.Assimp.Material #}
{#import Bindings.Assimp.Matrix4x4 #}
{#import Bindings.Assimp.Vector3 #}
{#import Bindings.Assimp.Anim #}

-- TODO include metadata I guess
data Node = Node { name'Node :: String
                 , transformation'Node :: Matrix4x4
                 , children'Node :: [Node]
                 , meshIxs'Node :: [Int]
                 } deriving (Show)

instance Storable Node where
  sizeOf _ = {#sizeof Node #}
  alignment _ = {#alignof Node #}
  peek p = do
    print "Node 0"
    AssimpString name <- peekByteOff p {#offsetof Node->mName #}
    print $ "Node name: " ++ name
    print "Node 1"
    transform <- peekByteOff p {#offsetof Node->mTransformation #}
    print "Node 2"
    numChildren <- liftM fromIntegral ({#get Node->mNumChildren #} p)
    print "Node 3"
    childrenPtr <- {#get Node->mChildren #} p
    print "Node 4"
    childPtrs <- peekArray numChildren (castPtr childrenPtr)
    print "Node 5"
    children <- mapM peek childPtrs
    print "Node 6"
    numMeshes <- liftM fromIntegral ({#get Node->mNumMeshes #} p)
    print "Node 7"
    meshIxsPtr <- {#get Node->mMeshes #} p
    print "Node 8"
    meshIxs <- peekArray numMeshes meshIxsPtr
    print "Node 9"
    return $ Node name transform children (map fromIntegral meshIxs)
  poke _ = undefined

data Scene = Scene { rootNode'Scene :: Node
                   , meshes'Scene :: [Mesh]
                   , materials'Scene :: [Material]
                   , animations'Scene :: [Animation]
                   , textures'Scene :: [Texture]
                   , lights'Scene :: [Light]
                   , cameras'Scene :: [Camera]
                   } deriving (Show)

instance Storable Scene where
  sizeOf _ = {#sizeof Scene #}
  alignment _ = {#alignof Scene #}
  peek p = do
    print "flag 0"
    rootNodePtr <- {#get Scene->mRootNode #} p
    rootNode <- peek $ castPtr rootNodePtr
    print "flag 1"
    numMeshes <- liftM fromIntegral ({#get Scene->mNumMeshes #} p)
    print "flag 1 1"
    meshesPtr <- {#get Scene->mMeshes #} p
    print "flag 1 2"
    meshPtrs <- peekArray numMeshes (castPtr meshesPtr)
    print "flag 1 3"
    meshes <- mapM peek meshPtrs
    print "flag 2"
    numMaterials <- liftM fromIntegral ({#get Scene->mNumMaterials #} p)
    materialsPtr <- {#get Scene->mMaterials #} p
    materialPtrs <- peekArray numMaterials (castPtr materialsPtr)
    materials <- mapM peek materialPtrs
    print "flag 3"
    numTextures <- liftM fromIntegral ({#get Scene->mNumTextures #} p)
    print $ "num textures: " ++ show numTextures
    texturesPtr <- {#get Scene->mTextures #} p
    texturePtrs <- peekArray numTextures (castPtr texturesPtr)
    textures <- mapM peek texturePtrs
    print "flag 4"
    numLights <- liftM fromIntegral ({#get Scene->mNumLights #} p)
    lightsPtr <- {#get Scene->mLights #} p
    lightPtrs <- peekArray numLights (castPtr lightsPtr)
    lights <- mapM peek lightPtrs
    print "flag 5"
    numCameras <- liftM fromIntegral ({#get Scene->mNumCameras #} p)
    camerasPtr <- {#get Scene->mCameras #} p
    cameraPtrs <- peekArray numCameras (castPtr camerasPtr)
    cameras <- mapM peek cameraPtrs

    numAnimations <- fromIntegral <$> ({#get Scene->mNumAnimations #} p)
    animationsPtr <- {#get Scene->mAnimations #} p
    animationPtrs <- peekArray numAnimations $ castPtr animationsPtr
    animations <- mapM peek animationPtrs

    return $ Scene rootNode meshes materials animations textures lights cameras

  poke _ = undefined

importFile :: String -> IO Scene
importFile filename =
  withCString filename $ \cstr -> do
    scenePtr <- {#call unsafe ImportFile #} cstr 0
    scene <- peek (castPtr scenePtr)
    {#call unsafe ReleaseImport #} scenePtr
    return scene
