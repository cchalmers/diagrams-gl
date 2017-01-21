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
-- Module      :  Diagrams.Backend.GL.Lines
-- Copyright   :  (c) 2017 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Primitives for drawing lines in the diagrams-gl backend.
--
module Diagrams.Backend.GL.Lines
  ( LineInfo (..)
  , LineProgram (..)
  , lineProgram
  , renderPath
  , drawLines
  ) where

import           Control.Monad.IO.Class
import qualified Data.Colour              as Colour
import           Data.FileEmbed
import qualified Data.Foldable            as F
import           Data.Maybe               (fromMaybe)
import           Data.Sequence            (Seq)
import qualified Data.Vector.Storable     as S
import           Foreign
import           Foreign.C                (withCString)
import           Linear                   (M44, transpose)

import           Graphics.GL

import           Diagrams.Prelude         hiding (clip, local, with, (<~))
import           Diagrams.Types           hiding (local)
import           Geometry.Path
import           Geometry.ThreeD.Types

import           Diagrams.Backend.GL.Util

------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------

-- | Infomation needed to draw a 3D line
data LineInfo = LineInfo
  { lineVao         :: !GLuint
  , lineNumSegments :: !Int
  , lineInfoWidth   :: !Float
  , lineInfoColour  :: !(Colour Double)
  , lineInfoModel   :: !(M44 Float)
  } deriving Show

-- | The program diagrams-gl uses to draw lines in 3D space that always
--   face the camera.
data LineProgram = LineProgram
  { lineProgramId    :: GLuint
  , lineViewId       :: GLint
  , lineProjectionId :: GLint
  , lineModelId      :: GLint
  , lineColourId     :: GLint
  , lineWidthId      :: GLint
  }

-- Rendering lines -----------------------------------------------------

-- | Turn a path into a vector of straight segments, splitting beziers
--   into a bunch of straight segments (not a very good way of rendering
--   beziers)
straightSegments :: Path V3 Double -> [S.Vector (P3 Float)]
straightSegments path = path ^.. each . to trailStraights

trailStraights :: Located (Trail V3 Double) -> S.Vector (P3 Float)
trailStraights t = S.fromList $ p : concatMap (map (fmap realToFrac) . seg2points) segs
  where
    segs = fixTrail t
    p = t ^. location . to (fmap realToFrac)
    seg2points (FLinear _ pnt) = [pnt]
    seg2points cs = map (cs `atParam`) [0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1]

-- Line program --------------------------------------------------------

lineProgram :: MonadIO m => m LineProgram
lineProgram = liftIO $ do
  let basicVert = $(embedFile "shaders/line-3D.vert")
      basicGeom = $(embedFile "shaders/line-3D.geom")
      basicFrag = $(embedFile "shaders/line-3D.frag")

  programId <- newProgram basicVert (Just basicGeom) basicFrag

  let getLoc nm = withCString nm $ glGetUniformLocation programId

  -- Set up transformation matrices
  uniModel  <- getLoc "model"
  uniView   <- getLoc "view"
  uniProj   <- getLoc "proj"
  uniColour <- getLoc "colour"
  uniWidth  <- getLoc "width"

  return LineProgram
    { lineProgramId    = programId
    , lineViewId       = uniView
    , lineProjectionId = uniProj
    , lineModelId      = uniModel
    , lineColourId     = uniColour
    , lineWidthId      = uniWidth
    }

renderPath :: T3 Double -> Path V3 Double -> Attributes -> IO LineInfo
renderPath t path attrs = do
  let pointData = S.concat (straightSegments $ transform t path)
  vao <- liftIO $ do
    vao <- allocRef $ glGenVertexArrays 1
    glBindVertexArray vao

    -- bind buffer to array

    -- Allocate and fill vertex buffer
    vertexBuffer <- allocRef $ glGenBuffers 1
    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    withStorable pointData $ \n ptr ->
      glBufferData GL_ARRAY_BUFFER n ptr GL_STATIC_DRAW

    -- Assign our buffers to vertex shader attributes
    glEnableVertexAttribArray 0
    glBindBuffer GL_ARRAY_BUFFER vertexBuffer
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (3*4) nullPtr

    return vao

  let width   = fromMaybe 0.01 (getAttr _LineWidth attrs) :: Double
      acolour = fromMaybe (opaque black) (getAttr _LineWidth attrs)
      -- alpha colours are not supported
      colour  = acolour `Colour.over` black
      modelMat = eye -- we transform before hand for now

--   putStrLn "rendering path!"
--   putStrLn $ "I calculated the width to be " ++ show width
--   putStrLn $ "getAttr _LineWidth = " ++ show (getAttr _LineWidth)
--   putStrLn $ "Attrs I received look like this:"
--   print (attrs^._Wrapped)
--   putStrLn []

  return LineInfo
    { lineVao         = vao
    , lineNumSegments = S.length pointData
    , lineInfoWidth   = realToFrac width
    , lineInfoColour  = colour
    , lineInfoModel   = modelMat
    }

-- | Draw the sequence of line infos using the line program and the
-- current projection and view matrix
drawLines :: SceneView -> LineProgram -> Seq LineInfo -> IO ()
drawLines (SceneView _ v p) lprog lineinfos = do
  glUseProgram (lineProgramId lprog)
  with (transpose v) $ glUniformMatrix4fv (lineViewId lprog) 1 GL_FALSE . castPtr
  with (transpose p) $ glUniformMatrix4fv (lineProjectionId lprog) 1 GL_FALSE . castPtr
  F.for_ lineinfos $ \line -> do

    glBindVertexArray (lineVao line)

    let m = lineInfoModel line
    with (transpose m) $ glUniformMatrix4fv (lineModelId lprog) 1 GL_FALSE . castPtr

    uniformColour (lineColourId lprog) (lineInfoColour line)
    glUniform1f (lineWidthId lprog) (lineInfoWidth line)

    glDrawArrays GL_LINES 0 (fromIntegral $ lineNumSegments line)

