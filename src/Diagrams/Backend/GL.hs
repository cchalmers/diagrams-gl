{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.GL
-- Copyright   :  (c) 2017 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The diagrams-gl backend is of two parts.
--
--   1. Generating a 'GLScene' from a diagram. A scene is made up of gl
--      primitivies.
--   2. Render a scene using 'glDraw' functions.
--
module Diagrams.Backend.GL
  ( -- * Generating a scene
    SceneInfo(..)
  , GLSceneM (..)
  , GLScene

    -- * Rendering a scene
  , RenderInfo
  , HasRenderInfo (..)
  , runRender
  , drawScene
  , diagramRender
  , toRender
  , renderAnnot
  , renderPrim
  , addBasicPrim
  , addPath
  , addLight

  -- * Text
  , GLText (..)
  , _GLText
  , pattern GLText_

  -- Mesh
  , BasicMesh (..)
  , basicMesh
  , _BasicMesh
  , pattern BasicMesh_

  , module Diagrams.Backend.GL.Basic
  , module Diagrams.Backend.GL.Lines
  , module Diagrams.Backend.GL.Util
  ) where

import           Control.Monad.IO.Class
import           Control.Monad.State
import qualified Data.Foldable              as F
import           Data.Maybe                 (fromMaybe)
import           Data.Sequence              (Seq)
import           Diagrams.Backend.Compile
import           Diagrams.Prelude           hiding (clip, local, with, (<~))
import           Diagrams.Types             hiding (local)
import           Geometry.ThreeD.Types

import Linear.V2 (V2 (..))
import Linear.Matrix
import Geometry.Space

import qualified Data.Vector.Storable as S

import           Geometry.Path

import           Graphics.GL

import           Diagrams.Backend.GL.Basic
import           Diagrams.Backend.GL.Lines
import           Diagrams.Backend.GL.Util
import           Diagrams.Backend.GL.Text

import Letters.Internal

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Infomation needed to render a diagram scene.
data SceneInfo = SceneInfo
  { _renderBasics :: Seq Basic
  , _renderLines  :: Seq LineInfo
  , _renderTexts  :: Seq TextInfo
  , _renderLights :: Seq (P3 Double, Colour Double)
  , _basicInfos   :: Seq BasicInfo
  , _sphereInfo   :: !BasicInfo
  , _cubeInfo     :: !BasicInfo
  , _ftFontFace   :: !FontFace
  , _renderGlyphs :: !GlyphMap
  } -- sphere and cube info don't really need to be here, they should
    -- really be in a reader portion

makeClassy ''SceneInfo

data RenderInfo = RenderInfo
  { _renderScene        :: SceneInfo
  , _renderBasicProgram :: BasicProgram
  , _renderLineProgram  :: LineProgram
  , _renderTextProgram  :: TextProgram
  }

makeClassy ''RenderInfo

instance HasSceneInfo RenderInfo where
  sceneInfo = renderScene

type GLScene = GLSceneM ()

-- | Monad used to compute the RenderInfo for rendering a diagram.
newtype GLSceneM a = GLScene (StateT SceneInfo IO a)
  deriving (Functor, Applicative, Monad, MonadState SceneInfo, MonadIO)

instance Semigroup GLScene where
  (<>) = (>>)
instance Monoid GLScene where
  mappend = (>>)
  mempty  = pure ()

initScene :: IO SceneInfo
initScene = do
  sphereI <- initSphere
  cubeI   <- initCube
  lib     <- newLibrary
  ff      <- newFontFace lib asana (16*64) 144
  return (SceneInfo mempty mempty mempty mempty mempty sphereI cubeI ff mempty)

deja :: FilePath
deja = "/Users/christopher/Documents/truetype/DejaVuSerif.ttf"


asana :: FilePath
asana = "/Users/christopher/Documents/truetype/letters/fonts/Asana-Math/Asana-Math.otf"

runRender :: GLScene -> IO SceneInfo
runRender (GLScene render) = do
  initScene >>= execStateT render

-- | Draw a scene from the RenderInfo with the provided view and
--   projection matrices.
drawScene :: SceneView -> RenderInfo -> IO ()
drawScene (SceneView sz viewMat projectionMat) rInfo = do

  -- let scene = _renderScene rInfo

  let mLight = preview (renderScene.renderLights._head) rInfo
  let (P lightP, _lightC) = fromMaybe (1000, orange) mLight
  uniform3v (lightPosID $ _renderBasicProgram rInfo) lightP
  -- uniformColour (lightColourID program) lightC

  glUseProgram (programID $ _renderBasicProgram rInfo)
  F.for_ (view (renderScene.renderBasics) rInfo)
         (renderBasic viewMat projectionMat (_renderBasicProgram rInfo))

  -- glUseProgram (lineProgramId $ _renderLineProgram rInfo)
  drawLines
    (SceneView sz viewMat projectionMat)
    (_renderLineProgram rInfo)
    (_renderLines (_renderScene rInfo))

  let mvp = projectionMat !*! viewMat

  -- This disables writing to the depth buffer but still respects
  -- it. This allows us to draw transparent values (which should
  -- be ordered) while still drawing them behind other objects
  glDepthMask GL_FALSE

  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  renderTextRenders
    sz
    mvp
    (rInfo^.renderTextProgram)
    (rInfo^.renderGlyphs)
    (rInfo^.renderTexts)

  glDepthMask GL_TRUE

diagramRender :: Diagram V3 -> IO RenderInfo
diagramRender dia = do
  prog     <- initBasicProgram
  lineProg <- lineProgram
  textProg <- initTextProgram
  scene    <- runRender (toRender mempty dia)
  return $ RenderInfo scene prog lineProg textProg

------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

toRender :: T3 Double -> Diagram V3 -> GLScene
toRender = foldDia renderPrim renderAnnot

renderAnnot :: Annotation V3 Double -> GLScene -> GLScene
renderAnnot _a = id
  -- | Just x <- getAnnot _GroupOpacity a = P.opacityGroup x
  -- | Just p <- getAnnot _Clip         a = clip (F.toList p)
  -- | otherwise                          = id

renderPrim :: T3 Double -> Attributes -> Prim V3 Double -> GLScene
renderPrim t@(T _ _ v) attrs = \case
  Cube_            -> use cubeInfo   >>= addBasicPrim t attrs
  Sphere_          -> use sphereInfo >>= addBasicPrim t attrs
  PointLight_ p c  -> addLight (papply t p) c
  Path_ p          -> addPath t p attrs
  GLText_ txt c    -> addText v txt c
  BasicMesh_ ps ns -> addMesh ps ns >>= addBasicPrim t attrs
  _                -> mempty

-- | All basic prims use the same shader.
addBasicPrim :: T3 Double -> Attributes -> BasicInfo -> GLScene
addBasicPrim t attrs info = do
  basic <- liftIO $ basicPrim t attrs info
  renderBasics %= cons basic

addLight :: P3 Double -> Colour Double -> GLScene
addLight p c = renderLights %= cons (p,c)

-- Text ----------------------------------------------------------------

data GLText = GLText String (AlphaColour Double)

type instance V GLText = V3
type instance N GLText = Double

_GLText :: Prism' (Prim V3 Double) GLText
_GLText = _Prim

pattern GLText_ :: String -> AlphaColour Double -> Prim V3 Double
pattern GLText_ t c <- (preview _GLText -> Just (GLText t c)) where
  GLText_ s c = Prim (GLText s c)

addText :: V3 Double -> String -> AlphaColour Double -> GLScene
addText v str c = do
  ff <- use ftFontFace

  txtInfo <- liftIO $ mkTextInfo ff str v c zero

  let codepoints = advancesCodepoints (textAdvances txtInfo)

  m  <- use renderGlyphs
  m' <- liftIO $ addGlyphs ff m codepoints
  renderGlyphs .= m'

  renderTexts %= cons txtInfo

-- Custom triangle mesh ------------------------------------------------

data BasicMesh = BasicMesh !(S.Vector (P3 Float)) !(S.Vector (V3 Float))

_BasicMesh :: Prism' (Prim V3 Double) BasicMesh
_BasicMesh = _Prim

pattern BasicMesh_ :: S.Vector (P3 Float) -> S.Vector (V3 Float) -> Prim V3 Double
pattern BasicMesh_ t c <- (preview _BasicMesh -> Just (BasicMesh t c)) where
  BasicMesh_ s c = Prim (BasicMesh s c)

type instance V BasicMesh = V3
type instance N BasicMesh = Double

addMesh
  :: S.Vector (P3 Float)
  -> S.Vector (V3 Float)
  -> GLSceneM BasicInfo
addMesh ps ns = do
  info  <- liftIO $ basicElement (S.unsafeCast ps) (S.unsafeCast ns)
  basicInfos %= cons info
  return info

basicMesh :: S.Vector (P3 Float) -> S.Vector (V3 Float) -> Diagram V3
basicMesh ts ns = mkQD (BasicMesh_ ts ns) env mempty mempty
  where
    -- VERY inefficient
    env = getEnvelope (toListOf (each.to (fmap realToFrac)) ts)

-- Rendering lines -----------------------------------------------------

addPath :: T3 Double -> Path V3 Double -> Attributes -> GLScene
addPath t path attrs = do
  info <- liftIO $ renderPath t path attrs
  renderLines %= cons info

