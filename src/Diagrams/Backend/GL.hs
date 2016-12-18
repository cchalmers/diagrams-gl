{-# LANGUAGE DeriveDataTypeable         #-}
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

import           Geometry.Path

import           Graphics.GL

import           Diagrams.Backend.GL.Basic
import           Diagrams.Backend.GL.Lines
import           Diagrams.Backend.GL.Util

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Infomation needed to render a diagram scene.
data SceneInfo = SceneInfo
  { _renderBasics :: Seq Basic
  , _renderLines  :: Seq LineInfo
  , _renderLights :: Seq (P3 Double, Colour Double)
  , _sphereInfo   :: !BasicInfo
  , _cubeInfo     :: !BasicInfo
  } -- sphere and cube info don't really need to be here, they should
    -- really be in a reader portion

makeLenses ''SceneInfo

data RenderInfo = RenderInfo
  { _renderScene        :: SceneInfo
  , _renderBasicProgram :: BasicProgram
  , _renderLineProgram  :: LineProgram
  }

makeClassy ''RenderInfo

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
  return (SceneInfo mempty mempty mempty sphereI cubeI)

runRender :: GLScene -> IO SceneInfo
runRender (GLScene render) = do
  initScene >>= execStateT render

-- | Draw a scene from the RenderInfo with the provided view and
--   projection matrices.
drawScene :: CameraMatrices -> RenderInfo -> IO ()
drawScene (CameraMatrices viewMat projectionMat) rInfo = do

  let mLight = preview (renderScene.renderLights._head) rInfo
  let (P lightP, _lightC) = fromMaybe (2, white) mLight
  uniform3v (lightPosID $ _renderBasicProgram rInfo) lightP
  -- uniformColour (lightColourID program) lightC

  glUseProgram (programID $ _renderBasicProgram rInfo)
  F.for_ (view (renderScene.renderBasics) rInfo) (renderBasic viewMat projectionMat (_renderBasicProgram rInfo))

  -- glUseProgram (lineProgramId $ _renderLineProgram rInfo)
  drawLines (CameraMatrices viewMat projectionMat) (_renderLineProgram rInfo) (_renderLines (_renderScene rInfo))

diagramRender :: Diagram V3 -> IO RenderInfo
diagramRender dia = do
  prog     <- initBasicProgram
  lineProg <- lineProgram
  scene    <- runRender (toRender mempty dia)
  return $ RenderInfo scene prog lineProg

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
renderPrim t attrs = \case
  Cube_           -> use cubeInfo   >>= addBasicPrim t attrs
  Sphere_         -> use sphereInfo >>= addBasicPrim t attrs
  PointLight_ p c -> addLight (papply t p) c
  Path_ p         -> addPath t p attrs
  _               -> mempty

-- | All basic prims use the same shader.
addBasicPrim :: T3 Double -> Attributes -> BasicInfo -> GLScene
addBasicPrim t attrs info = do
  basic <- liftIO $ basicPrim t attrs info
  renderBasics %= cons basic

addLight :: P3 Double -> Colour Double -> GLScene
addLight p c = renderLights %= cons (p,c)

-- Rendering lines -----------------------------------------------------

addPath :: T3 Double -> Path V3 Double -> Attributes -> GLScene
addPath t path attrs = do
  info <- liftIO $ renderPath t path attrs
  renderLines %= cons info

