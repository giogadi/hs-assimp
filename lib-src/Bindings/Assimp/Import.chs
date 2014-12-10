{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Import where

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

#include <assimp/postprocess.h>
#include <assimp/cimport.h>

{#context lib = "assimp" prefix = "ai" #}

{#import Bindings.Assimp.Scene #}

{#enum PostProcessSteps as PostProcessStep {} deriving (Eq, Read, Show, Ord) #}

stepSetToFlags :: S.Set PostProcessStep -> Word32
stepSetToFlags = S.foldr (.|.) 0 . S.map (fromIntegral . fromEnum)

convertToLeftHanded :: S.Set PostProcessStep
convertToLeftHanded = S.fromList [Process_MakeLeftHanded,
                                  Process_FlipUVs,
                                  Process_FlipWindingOrder]

postProcessRealTimeFast :: S.Set PostProcessStep
postProcessRealTimeFast = S.fromList [Process_CalcTangentSpace,
                                      Process_GenNormals,
                                      Process_JoinIdenticalVertices,
                                      Process_Triangulate,
                                      Process_GenUVCoords,
                                      Process_SortByPType]

postProcessRealTimeQuality :: S.Set PostProcessStep
postProcessRealTimeQuality = S.fromList [Process_CalcTangentSpace,
                                         Process_GenSmoothNormals,
                                         Process_JoinIdenticalVertices,
                                         Process_ImproveCacheLocality,
                                         Process_LimitBoneWeights,
                                         Process_RemoveRedundantMaterials,
                                         Process_SplitLargeMeshes,
                                         Process_Triangulate,
                                         Process_GenUVCoords,
                                         Process_SortByPType,
                                         Process_FindDegenerates,
                                         Process_FindInvalidData]

postProcessRealTimeMaxQuality :: S.Set PostProcessStep
postProcessRealTimeMaxQuality = S.fromList [Process_FindInstances,
                                            Process_ValidateDataStructure,
                                            Process_OptimizeMeshes]
                                  `S.union` postProcessRealTimeQuality

importFile :: String -> S.Set PostProcessStep -> IO Scene
importFile filename processSteps =
  withCString filename $ \cstr -> do
    let processFlags = stepSetToFlags processSteps
    print $ "post process " ++ show processFlags
    scenePtr <- {#call unsafe ImportFile #} cstr (fromIntegral processFlags)
    scene <- peek (castPtr scenePtr)
    {#call unsafe ReleaseImport #} scenePtr
    return scene

importFileDefault :: String -> IO Scene
importFileDefault filename = importFile filename $ S.fromList [Process_CalcTangentSpace,
                                                               Process_Triangulate,
                                                               Process_JoinIdenticalVertices,
                                                               Process_SortByPType]
