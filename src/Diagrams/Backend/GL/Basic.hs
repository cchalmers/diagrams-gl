{-# LANGUAGE BangPatterns               #-}
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
-- Module      :  Diagrams.Backend.GL.Basic
-- Copyright   :  (c) 2017 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Rendering basic shapes for the diagrams-gl backend.
--
module Diagrams.Backend.GL.Basic
  ( BasicInfo (..)
  , BasicProgram (..)
  , Basic (..)

  -- * Creating
  , initBasicProgram
  , initSphere
  , initCube
  , basicElement
  , basicPrim

  -- * Rendering
  , renderBasic
  ) where



import Diagrams.Types.Style
import Diagrams.ThreeD.Attributes
import           Diagrams.Prelude             hiding (clip, local, viewLoc,
                                               with, (<~))

import Data.Maybe (fromMaybe)
import           Data.FileEmbed
import qualified Data.Vector.Storable         as S
import qualified Data.Vector.Storable.Mutable as M
import           Foreign
import           Foreign.C                    (withCString)
import           Linear
import           Geometry.ThreeD.Types

import           Graphics.GL

import           Diagrams.Backend.GL.Util

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Vertex infomation about drawing a single shape.
data BasicInfo = BasicInfo
  { infoVao          :: !GLuint
  , infoNumTriangles :: !GLsizei
  , infoCleanup      :: IO ()
  }

-- | For now we only render very basic elements. This includes
--
--     - vertices
--     - normals
--     - a surface colour
--     - a single point light source
--     - ambient light
--     - model transformation
data Basic = Basic
  { basicInfo   :: !BasicInfo
  , basicColour :: !(Colour Double)
  -- , basicSpecular   :: !Float
  -- , basicAmbiend    :: !Float
  -- , basicDiffuse    :: !Float
  , basicModel  :: !(M44 Float)
  }

-- | The program used to render basic shapes (see 'Basic')
data BasicProgram = BasicProgram
  { programID     :: !GLuint
  , mvpID         :: !GLint
  , viewID        :: !GLint
  , modelID       :: !GLint
  , colourID      :: !GLint
  , lightPosID    :: !GLint
  , lightColourID :: !GLint
  -- , specularID :: GLint
  -- , diffuseID :: GLint
  }

initBasicProgram :: IO BasicProgram
initBasicProgram = do
  let basicVert = $(embedFile "shaders/shader-3D.vert")
      basicFrag = $(embedFile "shaders/shader-3D.frag")
  progID <- newProgram basicVert Nothing basicFrag
  glUseProgram progID

  let getLoc nm = withCString nm (glGetUniformLocation progID)

  mvpLoc    <- getLoc "MVP"
  viewLoc   <- getLoc "V"
  modelLoc  <- getLoc "M"
  lightPosLoc <- getLoc "LightPos_w"
  -- lightColourLoc <- getLoc "LightColour_w"
  colourLoc   <- getLoc "MaterialDiffuseColor"

  return BasicProgram
    { programID     = progID
    , mvpID         = mvpLoc
    , viewID        = viewLoc
    , modelID       = modelLoc
    , lightPosID    = lightPosLoc
    , lightColourID = 0
    , colourID      = colourLoc
    }

------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

basicPrim :: T3 Double -> Attributes -> BasicInfo -> IO Basic
basicPrim t attrs info = do
  let colour = getSC attrs
      model  = (\(T m _ v) -> mkTransformationMat m v) t

  return $ Basic
    { basicInfo    = info
    , basicColour  = colour
    , basicModel   = (fmap.fmap) realToFrac model
    }

getSC :: Attributes -> Colour Double
getSC = fromMaybe grey . getAttr _SurfaceColor

-- Rendering -----------------------------------------------------------

renderBasic :: M44 Float -> M44 Float -> BasicProgram -> Basic -> IO ()
renderBasic viewMat projectionMat ids basic = do
  let basicI = basicInfo basic
  glBindVertexArray (infoVao basicI)

  let modelMat = basicModel basic

  -- matricies
  with (transpose $ modelMat) $
    glUniformMatrix4fv (modelID ids) 1 GL_FALSE . castPtr
  with (transpose $ viewMat !*! modelMat) $
    glUniformMatrix4fv (viewID ids) 1 GL_FALSE . castPtr
  with (transpose $ projectionMat !*! viewMat !*! modelMat) $
    glUniformMatrix4fv (mvpID ids) 1 GL_FALSE . castPtr

  -- light position won't change for each object
  -- let P (V3 lx ly lz) = realToFrac <$> basicLightPos basic
  -- uniformV3 (lightID ids) lx ly lz

  -- colour of the shape
  uniformColour (colourID ids) (basicColour basic)

  glDrawArrays GL_TRIANGLES 0 (infoNumTriangles basicI)

-- Vertex info ---------------------------------------------------------

-- | Generate a vertex array object for a cube and a funtion that draws
--   the object.
basicElement
  :: S.Vector Float -- ^ vertices
  -> S.Vector Float -- ^ normals
  -> IO BasicInfo
basicElement vertexData normalData = do

  vao <- allocRef $ glGenVertexArrays 1
  glBindVertexArray vao

  -- Allocate and fill vertex buffer
  vertexBuffer <- allocRef $ glGenBuffers 1
  glBindBuffer GL_ARRAY_BUFFER vertexBuffer
  withStorable vertexData $ \n ptr ->
    glBufferData GL_ARRAY_BUFFER n ptr GL_STATIC_DRAW

  -- Allocate and fill normal buffer
  normalBuffer <- allocRef $ glGenBuffers 1
  glBindBuffer GL_ARRAY_BUFFER normalBuffer
  withStorable normalData $ \n ptr ->
    glBufferData GL_ARRAY_BUFFER n ptr GL_STATIC_DRAW

  -- Assign our buffers to vertex shader attributes
  glEnableVertexAttribArray 0
  glBindBuffer GL_ARRAY_BUFFER vertexBuffer
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr

  glEnableVertexAttribArray 1
  glBindBuffer GL_ARRAY_BUFFER normalBuffer
  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE 0 nullPtr

  -- Unbind buffer
  glBindBuffer GL_ARRAY_BUFFER 0

  let cleanup = do
        with vertexBuffer $ glDeleteBuffers 1
        with normalBuffer $ glDeleteBuffers 1
        with vao $ glDeleteVertexArrays 1

  let numTriangles = fromIntegral $ 3*S.length vertexData

  glBindVertexArray 0
  return (BasicInfo vao numTriangles cleanup)

-- Basic shapes --------------------------------------------------------

-- | Initiate a sphere generated by teselating a 12*12 cube. With center
--   at the origin and radius 1.
initSphere :: IO BasicInfo
initSphere = do
  vao <- allocRef $ glGenVertexArrays 1
  glBindVertexArray vao

  vertexBuffer <- allocRef $ glGenBuffers 1
  glBindBuffer GL_ARRAY_BUFFER vertexBuffer
  storeableBufferData GL_ARRAY_BUFFER teselatedCube GL_STATIC_DRAW

  -- vertices in attribute 0
  glEnableVertexAttribArray 0
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 nullPtr

  -- normals in attribute 1 (normals=vertices for sphere)
  glEnableVertexAttribArray 1
  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE 0 nullPtr

  glBindBuffer GL_ARRAY_BUFFER 0

  let numTriangles = fromIntegral $ 3*S.length teselatedCube

  let cleanup = do
        with vertexBuffer $ glDeleteBuffers 1
        with vao $ glDeleteVertexArrays 1

  glBindVertexArray 0
  return (BasicInfo vao numTriangles cleanup)


-- | We start with a cube where is face is made up of an 8x8 grid of
--   quads with the center of the cube at the origin. We then normalise
--   each vertex making up this cube so the distance from the origin is
--   1. This is what we use for our approximate sphere.
--
--   Also note the normal to the sphere at each vertex is the same as
--   the position of the vertex.
teselatedCube :: S.Vector (V3 Float)
teselatedCube = S.create $ do
  let xMax = 12
  let yMax = 12
  mv <- M.new (xMax*yMax*6*6)
  let fromI = fromIntegral :: Int -> Float
  let makePlanes !plane !f = go (12*xMax*yMax*plane) 0 0 where
        g x y z = normalize $ f (2* (fromI (x-xMax`div`2)) / fromI xMax)
                                (2* (fromI (y-yMax`div`2)) / fromI yMax)
                                z
        go !i !x !y = do
          -- draw the two triangles making up this grid site
          M.write mv  i     (g  x    y    1)
          M.write mv (i+1)  (g (x+1) y    1)
          M.write mv (i+2)  (g  x   (y+1) 1)
          M.write mv (i+3)  (g  x   (y+1) 1)
          M.write mv (i+4)  (g (x+1) y    1)
          M.write mv (i+5)  (g (x+1)(y+1) 1)

          M.write mv (i+11) (g  x    y    (-1))
          M.write mv (i+10) (g (x+1) y    (-1))
          M.write mv (i+9)  (g  x   (y+1) (-1))
          M.write mv (i+8)  (g  x   (y+1) (-1))
          M.write mv (i+7)  (g (x+1) y    (-1))
          M.write mv (i+6)  (g (x+1)(y+1) (-1))

          if | x < xMax-1 -> go (i+12) (x+1) y
             | y < yMax-1 -> go (i+12)  0   (y+1)
             | otherwise  -> return ()

  makePlanes 0 (\x y z -> V3 x y z )
  makePlanes 1 (\x y z -> V3 y z x )
  makePlanes 2 (\x y z -> V3 z x y )

  return mv

-- | Initiate a cube with centre at the origin and sides of length 1.
initCube :: IO BasicInfo
initCube = basicElement cubeVertices cubeNormals
  where
    cubeVertices = S.map (*0.5) $ S.fromList
      [  1, 1,-1,  1,-1,-1, -1,-1,-1
      ,  1, 1,-1, -1,-1,-1, -1, 1,-1
      , -1,-1, 1, -1, 1, 1, -1, 1,-1
      , -1,-1, 1, -1, 1,-1, -1,-1,-1
      ,  1,-1, 1,  1, 1, 1, -1,-1, 1
      ,  1, 1, 1, -1, 1, 1, -1,-1, 1
      ,  1,-1,-1,  1, 1,-1,  1,-1, 1
      ,  1, 1,-1,  1, 1, 1,  1,-1, 1
      ,  1, 1,-1, -1, 1,-1,  1, 1, 1
      , -1, 1,-1, -1, 1, 1,  1, 1, 1
      ,  1,-1,-1,  1,-1, 1, -1,-1, 1
      ,  1,-1,-1, -1,-1, 1, -1,-1,-1
      ]
    cubeNormals = S.fromList
      [  0, 0,-1,  0, 0,-1,  0, 0,-1
      ,  0, 0,-1,  0, 0,-1,  0, 0,-1
      , -1, 0, 0, -1, 0, 0, -1, 0, 0
      , -1, 0, 0, -1, 0, 0, -1, 0, 0
      ,  0, 0, 1,  0, 0, 1,  0, 0, 1
      ,  0, 0, 1,  0, 0, 1,  0, 0, 1
      ,  1, 0, 0,  1, 0, 0,  1, 0, 0
      ,  1, 0, 0,  1, 0, 0,  1, 0, 0
      ,  0, 1, 0,  0, 1, 0,  0, 1, 0
      ,  0, 1, 0,  0, 1, 0,  0, 1, 0
      ,  0,-1, 0,  0,-1, 0,  0,-1, 0
      ,  0,-1, 0,  0,-1, 0,  0,-1, 0
      ]

