{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Mesh where

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

#include <assimp/mesh.h>

{#context lib = "assimp" prefix = "ai" #}

{#import Bindings.Assimp.Types #}
{#import Bindings.Assimp.Vector2 #}
{#import Bindings.Assimp.Vector3 #}
{#import Bindings.Assimp.Matrix4x4 #}

{#enum PrimitiveType {PrimitiveType_POINT as Point,
                      PrimitiveType_LINE as Line,
                      PrimitiveType_TRIANGLE as Triangle,
                      PrimitiveType_POLYGON as Polygon,
                      _aiPrimitiveType_Force32Bit as InvalidPrimitiveType} deriving (Eq, Read, Show, Ord) #}

data Vertex = Vertex { position :: Vector3D
                     , normal :: Maybe Vector3D
                     , tangent :: Maybe Vector3D
                     , bitangent :: Maybe Vector3D
                     , color :: [Color4D]
                     , textureCoord :: [Vector3D]
                     } deriving (Show)

data Face = Face [Int] deriving (Eq, Read, Show)

instance Storable Face where
  sizeOf _ = {#sizeof Face #}
  alignment _ = {#alignof Face #}
  peek p = do
    numIndices <- fromIntegral <$> ({#get Face->mNumIndices #} p)
    indicesPtr <- {#get Face->mIndices #} p
    indices <- peekArray numIndices (castPtr indicesPtr) :: IO [Word32]
    return $ Face $ map fromIntegral indices
  poke p = undefined

data VertexWeight = VertexWeight { _vertexIx :: Int
                                 , _weight :: Float
                                 } deriving (Eq, Read, Show)

instance Storable VertexWeight where
  sizeOf _ = {#sizeof VertexWeight #}
  alignment _ = {#alignof VertexWeight #}
  peek p = VertexWeight
    <$> (fromIntegral <$> ({#get VertexWeight->mVertexId #} p))
    <*> (realToFrac <$> ({#get VertexWeight->mWeight #} p))
  poke _ = undefined

data Bone = Bone { name'Bone :: String
                 , vertexWeights :: [VertexWeight]
                 , offsetMatrix :: Matrix4x4
                 } deriving (Show)

instance Storable Bone where
  sizeOf _ = {#sizeof Bone #}
  alignment _ = {#alignof Bone #}
  peek p = do
    AssimpString name <- peekByteOff p {#offsetof Bone->mName #}
    numWeights <- fromIntegral <$> ({#get Bone->mNumWeights #} p)
    weightsPtr <- {#get Bone->mWeights #} p
    weights <- peekArray numWeights (castPtr weightsPtr)
    matrix <- peekByteOff p {#offsetof Bone->mOffsetMatrix #}
    return $ Bone name weights matrix
  poke _ = undefined

-- TODO SRSLY use a vector for vertices idiot
data Mesh = Mesh { primitiveTypes'Mesh :: S.Set PrimitiveType
                 , vertices'Mesh :: [Vertex]
                 , faces'Mesh :: [Face]
                 , numUVComponents'Mesh :: [Int]
                 , bones'Mesh :: [Bone]
                 , materialIndex'Mesh :: Int
                 , name'Mesh :: String
                 } deriving (Show)

instance Storable Mesh where
  sizeOf _ = {#sizeof Mesh #}
  alignment _ = {#alignof Mesh #}
  peek p = do
    print "Mesh 1"
    typeFlags <- fromIntegral <$> ({#get Mesh->mPrimitiveTypes #} p)
    print typeFlags
    print "Mesh 2"
    let primitiveTypeSet = S.fromList $ filter ((/= 0) . (typeFlags .&.) . fromEnum) $ enumFrom Point
    print "Mesh 3"
    numVertices <- fromIntegral <$> ({#get Mesh->mNumVertices #} p)
    print $ "num vertices: " ++ show numVertices
    print "Mesh 4"
    positionPtr <- {#get Mesh->mVertices #} p
    print "Mesh 5"
    positions <- peekArray numVertices (castPtr positionPtr)
    print "Mesh 6"
    normalsPtr <- {#get Mesh->mNormals #} p
    print "Mesh 7"
    normals <- if normalsPtr == nullPtr
                 then return $ repeat Nothing
                 else (liftM $ map Just) $ peekArray numVertices (castPtr normalsPtr)
    print "Mesh 8"
    tangentsPtr <- {#get Mesh->mTangents #} p
    print "Mesh 9"
    tangents <- if tangentsPtr == nullPtr
                  then return $ repeat Nothing
                  else (liftM $ map Just) $ peekArray numVertices (castPtr tangentsPtr)
    print "Mesh 10"
    bitangentsPtr <- {#get Mesh->mBitangents #} p
    print "Mesh 11"
    bitangents <- if bitangentsPtr == nullPtr
                    then return $ repeat Nothing
                    else (liftM $ map Just) $ peekArray numVertices (castPtr bitangentsPtr)
    print "Mesh 12"
    let colorSetsOffset = {#offsetof Mesh->mColors #}
    print $ "color offset " ++ show colorSetsOffset
    print "Mesh 13"
    let colorPtrOffsets = take {#const AI_MAX_NUMBER_OF_COLOR_SETS #} [0, sizeOf nullPtr ..]
    colorSetPtrs <- mapM (peekByteOff p) (map (colorSetsOffset +) colorPtrOffsets)
    print "Mesh 14"
    let usedColorSetPtrs = filter (/= nullPtr) colorSetPtrs
    print "Mesh 15"
    colorSets <- mapM (peekArray numVertices . castPtr) usedColorSetPtrs
    print "Mesh 16"
    let textureCoordSetsOffset = {#offsetof Mesh->mTextureCoords #}
    print $ "texture coords offset " ++ show textureCoordSetsOffset
    let textureCoordPtrOffsets = take {#const AI_MAX_NUMBER_OF_TEXTURECOORDS #} [0, sizeOf nullPtr .. ]
    textureCoordSetPtrs <- mapM (peekByteOff p) (map (textureCoordSetsOffset +) textureCoordPtrOffsets)
    print "Mesh 18"
    let usedTextureCoordSetPtrs = filter (/= nullPtr) textureCoordSetPtrs
    textureCoordSets <- mapM (peekArray numVertices . castPtr) usedTextureCoordSetPtrs
    print "Mesh 19"
    let colorSetsTransposed = if null colorSets
                                then repeat []
                                else transpose colorSets
    let textureCoordSetsTransposed = if null textureCoordSets
                                       then repeat []
                                       else transpose textureCoordSets
    let vertices = zipWith6 Vertex positions normals tangents bitangents colorSetsTransposed textureCoordSetsTransposed
    let numUVCompsOffset = {#offsetof Mesh->mNumUVComponents #}
    let numUVCompsOffsets = take {#const AI_MAX_NUMBER_OF_TEXTURECOORDS #} [0, (sizeOf (undefined :: Word32)) ..]
    print "Mesh 20"
    numUVComponents <- mapM (peekByteOff p) (map (numUVCompsOffset +) numUVCompsOffsets)
    print "Mesh 21"
    numFaces <- fromIntegral <$> ({#get Mesh->mNumFaces #} p)
    print "Mesh 22"
    facesPtr <- {#get Mesh->mFaces #} p
    print "Mesh 23"
    faces <- peekArray numFaces (castPtr facesPtr)
    print "Mesh 24"
    materialIndex <- fromIntegral <$> ({#get Mesh->mMaterialIndex #} p)
    print "Mesh 25"
    (AssimpString name) <- peekByteOff p {#offsetof Mesh->mName #}
    print "Mesh 26"
    numBones <- fromIntegral <$> ({#get Mesh->mNumBones #} p)
    bonesPtr <- {#get Mesh->mBones #} p
    bonePtrs <- peekArray numBones (castPtr bonesPtr)
    bones <- mapM peek bonePtrs
    return $ Mesh primitiveTypeSet vertices faces numUVComponents bones materialIndex name
  poke p = undefined
