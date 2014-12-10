{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Assimp.Material where

import Control.Applicative
import Control.Monad
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
                    , path'TextureProperties :: Maybe String
                    , mapping'TextureProperties :: Maybe TextureMapping
                    , uvIndex'TextureProperties :: Maybe Int
                    , blend'TextureProperties :: Maybe Float
                    , op'TextureProperties :: Maybe TextureOp
                    , mapMode'TextureProperties :: Maybe TextureMapMode
                    , invertColors' :: Maybe Bool
                    , useAlpha' :: Maybe Bool
                    } deriving (Show)

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
      , textureProperties'Material = [] -- TODO FINISH THIS IDIOT
      }
  poke p = undefined
