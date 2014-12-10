{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Material where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable

#include <assimp/material.h>

{#context lib = "assimp" prefix = "ai" #}

{#import Bindings.Assimp.Vector2 #}
{#import Bindings.Assimp.Types #}

{#enum TextureOp {TextureOp_Multiply as TextureOpMultiply,
                  TextureOp_Add as TextureOpAdd,
                  TextureOp_Subtract as TextureOpSubtract,
                  TextureOp_Divide as TextureOpDivide,
                  TextureOp_SmoothAdd as TextureOpSmoothAdd,
                  TextureOp_SignedAdd as TextureOpSignedAdd,
                   _aiTextureOp_Force32Bit as InvalidTextureOp} deriving (Eq, Read, Show) #}

{#enum TextureMapMode {TextureMapMode_Wrap as TextureMapModeWrap,
                       TextureMapMode_Clamp as TextureMapModeClamp,
                       TextureMapMode_Decal as TextureMapModeDecal,
                       TextureMapMode_Mirror as TextureMapModeMirror,
                       _aiTextureMapMode_Force32Bit as InvalidTextureMapMode} deriving (Eq, Read, Show) #}

{#enum TextureMapping {TextureMapping_UV as TextureMappingUV,
                       TextureMapping_SPHERE as TextureMappingSphere,
                       TextureMapping_CYLINDER as TextureMappingCylinder,
                       TextureMapping_BOX as TextureMappingBox,
                       TextureMapping_PLANE as TextureMappingPlane,
                       TextureMapping_OTHER as TextureMappingOther,
                       _aiTextureMapping_Force32Bit as InvalidTextureMapping} deriving (Eq, Read, Show) #}

{#enum TextureType {TextureType_NONE as TextureTypeNone,
                    TextureType_DIFFUSE as TextureTypeDiffuse,
                    TextureType_SPECULAR as TextureTypeSpecular,
                    TextureType_AMBIENT as TextureTypeAmbient,
                    TextureType_EMISSIVE as TextureTypeEmissive,
                    TextureType_HEIGHT as TextureTypeHeight,
                    TextureType_NORMALS as TextureTypeNormals,
                    TextureType_SHININESS as TextureTypeShininess,
                    TextureType_OPACITY as TextureTypeOpacity,
                    TextureType_DISPLACEMENT as TextureTypeDisplacement,
                    TextureType_LIGHTMAP as TextureTypeLightmap,
                    TextureType_REFLECTION as TextureTypeReflection,
                    TextureType_UNKNOWN as TextureTypeUnknown,
                    _aiTextureType_Force32Bit as InvalidTextureType} deriving (Eq, Read, Show) #}

{#enum ShadingMode {ShadingMode_Flat as ShadingModeFlat,
                    ShadingMode_Gouraud as ShadingModeGouraud,
                    ShadingMode_Phong as ShadingModePhong,
                    ShadingMode_Blinn as ShadingModeBlinn,
                    ShadingMode_Toon as ShadingModeToon,
                    ShadingMode_OrenNayar as ShadingModeOrenNayar,
                    ShadingMode_Minnaert as ShadingModeMinnaert,
                    ShadingMode_CookTorrance as ShadingModeCookTorrance,
                    ShadingMode_NoShading as ShadingModeNoShading,
                    ShadingMode_Fresnel as ShadingModeFresnel,
                    _aiShadingMode_Force32Bit as InvalidShadingMode} deriving (Eq, Read, Show) #}

{#enum TextureFlags {TextureFlags_Invert as TextureFlagsInvert,
                     TextureFlags_UseAlpha as TextureFlagsUseAlpha,
                     TextureFlags_IgnoreAlpha as TextureFlagsIgnoreAlpha,
                     _aiTextureFlags_Force32Bit as InvalidTextureFlags} deriving (Eq, Read, Show) #}

{#enum BlendMode {BlendMode_Default as BlendModeDefault,
                  BlendMode_Additive as BlendModeAdditive,
                  _aiBlendMode_Force32Bit as InvalidBlendMode} deriving (Eq, Read, Show) #}

{#enum PropertyTypeInfo {PTI_Float as PTIFloat,
                         PTI_String as PTIString,
                         PTI_Integer as PTIInteger,
                         PTI_Buffer as PTIBuffer,
                         _aiPTI_Force32Bit as InvalidPTI} deriving (Eq, Read, Show) #}

data MaterialProperties =
  MaterialProperties { name'MaterialProperties :: Maybe String
                     , twoSided'MaterialProperties :: Maybe Bool
                     , shadingModel'MaterialProperties :: Maybe ShadingMode
                     , enableWireframe'MaterialProperties :: Maybe Bool
                     , blendFunc'MaterialProperties :: Maybe BlendMode
                     , opacity'MaterialProperties :: Maybe Float
                     , bumpScaling'MaterialProperties :: Maybe Float
                     , shininess'MaterialProperties :: Maybe Float
                     , reflectivity'MaterialProperties :: Maybe Float
                     , shininessStrength'MaterialProperties :: Maybe Float
                     , refractIndex'MaterialProperties :: Maybe Float
                     , colorDiffuse'MaterialProperties :: Maybe Color4D
                     , colorAmbient'MaterialProperties :: Maybe Color4D
                     , colorSpecular'MaterialProperties :: Maybe Color4D
                     , colorEmissive'MaterialProperties :: Maybe Color4D
                     , colorTransparent'MaterialProperties :: Maybe Color4D
                     , colorReflective'MaterialProperties :: Maybe Color4D
                     , globalBackgroundImage'MaterialProperties :: Maybe String
                     } deriving (Show)

data TextureProperties =
  TextureProperties { type'TextureProperties :: TextureType
                    , path'TextureProperties :: String
                    -- These are put in time-out because assimp is dumb
                    -- , mapping'TextureProperties :: TextureMapping
                    -- , uvIndex'TextureProperties :: Int
                    -- , blend'TextureProperties :: Float
                    -- , op'TextureProperties :: TextureOp
                    -- , mapModeU'TextureProperties :: TextureMapMode
                    -- , mapModeV'TextureProperties :: TextureMapMode
                    -- TODO decide what to do with this shit
                    -- , invertColors' :: Maybe Bool
                    -- , useAlpha' :: Maybe Bool
                    } deriving (Show)

-- TODO: consider making textureProperties a map from texture type to properties.
data Material = Material { materialProperties'Material :: MaterialProperties
                         , textureProperties'Material :: [TextureProperties] }
                         deriving (Show)

getMaterialString :: Ptr Material -> String -> IO (Maybe String)
getMaterialString p key =
  withCString key $ \cstr ->
    alloca $ \aiStrPtr -> do
      returnCode <- liftM (toEnum . fromIntegral) $ {#call unsafe GetMaterialString #}
        (castPtr p) cstr 0 0 (castPtr aiStrPtr)
      case returnCode of
        Success -> do
          (AssimpString str) <- peek aiStrPtr
          return $ Just str
        Failure -> return Nothing
        _ -> error $ "error getMaterialName: " ++ key

getMaterialIntegral :: Ptr Material -> String -> IO (Maybe Word32)
getMaterialIntegral p key =
  withCString key $ \cstr ->
    alloca $ \intPtr -> do
      returnCode <- liftM (toEnum . fromIntegral) $ {#call unsafe GetMaterialIntegerArray #}
        (castPtr p) cstr 0 0 (castPtr intPtr) nullPtr
      case returnCode of
        Success -> liftM Just $ peek intPtr
        Failure -> return Nothing
        _ -> error $ "error getMaterialIntegral: " ++ key

getMaterialFloat :: Ptr Material -> String -> IO (Maybe Float)
getMaterialFloat p key =
  withCString key $ \cstr ->
    alloca $ \floatPtr -> do
      returnCode <- liftM (toEnum . fromIntegral) $ {#call unsafe GetMaterialFloatArray #}
        (castPtr p) cstr 0 0 (castPtr floatPtr) nullPtr
      case returnCode of
        Success -> liftM Just $ peek floatPtr
        Failure -> return Nothing
        _ -> error $ "error getMaterialFloat: " ++ key

getMaterialColor :: Ptr Material -> String -> IO (Maybe Color4D)
getMaterialColor p key =
  withCString key $ \cstr ->
    alloca $ \colorPtr -> do
      returnCode <- liftM (toEnum . fromIntegral) $ {#call unsafe GetMaterialColor #}
        (castPtr p) cstr 0 0 (castPtr colorPtr)
      case returnCode of
        Success -> liftM Just $ peek colorPtr
        Failure -> return Nothing
        _ -> error $ "error getMaterialColor: " ++ key

-- Put in timeout because assimp doesn't seem to have a way of telling us whether it has given us valid values for stuff.
-- getMaterialTexture :: Ptr Material -> TextureType -> Int -> IO (Maybe TextureProperties)
-- getMaterialTexture p texType ix =
--   alloca $ \pathPtr ->
--     alloca $ \texMappingPtr ->
--       alloca $ \uvIxPtr ->
--         alloca $ \blendPtr ->
--           alloca $ \opPtr ->
--             allocaArray 2 $ \mapModePtr ->
--               alloca $ \flagsPtr -> do
--                 returnCode <- liftM (toEnum . fromIntegral) $ {#call unsafe GetMaterialTexture #}
--                   (castPtr p) (fromIntegral $ fromEnum texType) (fromIntegral ix)
--                   (castPtr pathPtr) (castPtr texMappingPtr) (castPtr uvIxPtr)
--                   (castPtr blendPtr) (castPtr opPtr) (castPtr mapModePtr) (castPtr flagsPtr)
--                 case returnCode of
--                   Success -> do
--                     AssimpString path <- peek pathPtr
--                     texMapping <- liftM (toEnum . fromIntegral) (peek texMappingPtr :: IO CUInt)
--                     uvIx <- fromIntegral <$> (peek uvIxPtr :: IO Word32)
--                     blend <- realToFrac <$> (peek blendPtr :: IO CFloat)
--                     op <- liftM (toEnum . fromIntegral) (peek opPtr :: IO Word8)
--                     [mapModeU, mapModeV] <- fmap (map (toEnum . fromIntegral)) (peekArray 2 mapModePtr :: IO [CUInt])
--                     flags <- peek flagsPtr :: IO (CUInt)
--                     return $ Just $ TextureProperties
--                       texType path texMapping uvIx blend op mapModeU mapModeV
--                   Failure -> return Nothing
--                   _ -> error $ "error getMaterialTexture: " ++ show texType ++ " " ++ show ix

getMaterialTexture :: Ptr Material -> TextureType -> Int -> IO (Maybe TextureProperties)
getMaterialTexture p texType ix =
  alloca $ \pathPtr -> do
    returnCode <- liftM (toEnum . fromIntegral) $ {#call unsafe GetMaterialTexture #}
                    (castPtr p) (fromIntegral $ fromEnum texType) (fromIntegral ix)
                    (castPtr pathPtr) nullPtr nullPtr
                    nullPtr nullPtr nullPtr nullPtr
    case returnCode of
      Success -> do
        AssimpString path <- peek pathPtr
        return $ Just $ TextureProperties texType path
      Failure -> return Nothing
      _ -> error $ "error getMaterialTexture: " ++ show texType ++ " " ++ show ix

getMaterialTextureCount :: Ptr Material -> TextureType -> IO (Int)
getMaterialTextureCount p texType =
  fromIntegral <$> {#call unsafe GetMaterialTextureCount #} (castPtr p) (fromIntegral $ fromEnum texType)

getMaterialTextures :: Ptr Material -> IO [TextureProperties]
getMaterialTextures p = do
  let types = enumFromTo TextureTypeDiffuse TextureTypeUnknown
  counts <- mapM (getMaterialTextureCount p) types
  let indicesToCheck = map (\c -> [0 .. c - 1]) counts
      texturesFromType t ixs = catMaybes <$> mapM (getMaterialTexture p t) ixs
  concat <$> sequence (zipWith texturesFromType types indicesToCheck)

instance Storable Material where
  sizeOf _ = {#sizeof Material #}
  alignment _ = {#alignof Material #}
  peek p = do
    name <- getMaterialString p "?mat.name"
    twosided <- getMaterialIntegral p "$mat.twosided"
    shadingModel <- fmap (fmap fromIntegral) $ getMaterialIntegral p "$mat.shadingm"
    wireframe <- getMaterialIntegral p "$mat.wireframe"
    blend <- fmap (fmap fromIntegral) $ getMaterialIntegral p "$mat.blend"
    opacity <- getMaterialFloat p "$mat.opacity"
    bumpscaling <- getMaterialFloat p "$mat.bumpscaling"
    shininess <- getMaterialFloat p "$mat.shininess"
    reflectivity <- getMaterialFloat p "$mat.reflectivity"
    shininessStr <- getMaterialFloat p "$mat.shinpercent"
    refracti <- getMaterialFloat p "$mat.refracti"
    diffuse <- getMaterialColor p "$clr.diffuse"
    ambient <- getMaterialColor p "$clr.ambient"
    specular <- getMaterialColor p "$clr.specular"
    emissive <- getMaterialColor p "$clr.emissive"
    transparent <- getMaterialColor p "$clr.transparent"
    reflective <- getMaterialColor p "$clr.reflective"
    bgImage <- getMaterialString p "?bg.global"
    textures <- getMaterialTextures p
    return $ Material
      { materialProperties'Material =
          MaterialProperties name
                             ((fmap (/= 0)) twosided)
                             ((fmap toEnum) shadingModel)
                             ((fmap (/= 0)) wireframe)
                             ((fmap toEnum) blend)
                             opacity
                             bumpscaling
                             shininess
                             reflectivity
                             shininessStr
                             refracti
                             diffuse
                             ambient
                             specular
                             emissive
                             transparent
                             reflective
                             bgImage
      , textureProperties'Material = textures
      }
  poke p = undefined
