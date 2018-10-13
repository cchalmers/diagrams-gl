{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.GL
-- Copyright   :  (c) 2016 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The SDL backend comes in a few different levels. The simplest case
--
-- Mainable useage takes a diagram and loads a sdl window with it.

module Diagrams.Backend.GL.Texture where

import           Data.FileEmbed

import           Control.Concurrent
import           Control.Lens
import           Control.Monad        (unless, when)
import           Control.Monad.State  hiding (get)
import qualified Data.Foldable        as F
import           Foreign              hiding (rotate)
import           Graphics.GL
import           Linear
import           Linear.Affine

import qualified Data.ByteString      as BS

import           Foreign.C

import           Codec.Picture
import qualified Data.Vector.Storable as S

roundUpBinary :: Int -> Int
roundUpBinary n = 1 + foldr go (n-1) [32,16,8,4,2,1]
  where go i m = m .|. m `shiftR` i

tex8 :: Image Pixel8 -> IO GLuint
tex8 (Image w h vec) = do
  S.unsafeWith vec $ \ptr -> do
    -- GLuint textureID
    -- glGenTextures 1 textureID
    textureID <- allocRef $ glGenTextures 1

    -- "Bind" the newly created texture : all future texture functions
    -- will modify this texture
    glBindTexture GL_TEXTURE_2D textureID

    -- Give the image to OpenGL
    glTexImage2D
      GL_TEXTURE_2D
      0
      GL_R8
      (fromIntegral w)
      (fromIntegral h)
      0
      GL_RED
      GL_UNSIGNED_BYTE
      (castPtr ptr)

    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST

    return textureID

startTexture :: Image PixelRGB8 -> IO GLuint
startTexture (Image w h vec) = do
  S.unsafeWith vec $ \ptr -> do
    -- GLuint textureID
    -- glGenTextures 1 textureID
    textureID <- allocRef $ glGenTextures 1

    -- "Bind" the newly created texture : all future texture functions
    -- will modify this texture
    glBindTexture GL_TEXTURE_2D textureID

    -- Give the image to OpenGL
    glTexImage2D
      GL_TEXTURE_2D
      0
      GL_RGB
      (fromIntegral w)
      (fromIntegral h)
      0
      GL_RGB
      GL_UNSIGNED_BYTE
      (castPtr ptr)

    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST

    return textureID

-- gl :: SDL.Window -> IO ()
-- gl window = do

--   glEnable GL_DEPTH_TEST
--   glDepthFunc GL_LESS
--   glEnable GL_CULL_FACE
--   glEnable GL_BLEND
--   glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

--   glClearColor 0 0 0 1
--   glClear $  GL_COLOR_BUFFER_BIT
--          .|. GL_STENCIL_BUFFER_BIT
--          .|. GL_DEPTH_BUFFER_BIT

--   -- the program
--   -- let basicVert = $(embedFile "texture.vert")
--   --     basicFrag = $(embedFile "texture.frag")
--   basicVert <- BS.readFile "texture.vert"
--   basicFrag <- BS.readFile "texture.frag"
--   programID <- newProgram basicVert Nothing basicFrag
--   glUseProgram programID

--   let getLoc nm = withCString nm (glGetUniformLocation programID)

--   mvpLoc    <- getLoc "MVP"
--   -- viewLoc   <- getLoc "V"
--   -- modelLoc  <- getLoc "M"

--   with (identity :: M44 Float) $
--     glUniformMatrix4fv mvpLoc 1 GL_FALSE . castPtr

--   let getLoc nm = withCString nm (glGetUniformLocation programID)

--   -- Right (ImageRGB8 img) <- readImage "/Users/christopher/Documents/truetype/freetype-gl/doc/images/lcd.png"

--   texture <- tex8 myImage
--   textureID <- withCString "myTextureSampler" $ glGetUniformLocation programID
--   -- glActiveTexture GL_TEXTURE0
--   -- glBindTexture GL_TEXTURE_2D texture

--   -- Set our "myTextureSampler" sampler to user Texture Unit 0
--   glUniform1i textureID 0

--   -- Vertex for the rect that holds the text ---------------------------

--   vao <- allocRef $ glGenVertexArrays 1
--   glBindVertexArray vao

--   -- let vertexBufferData = S.fromList
--   --       [ 0,0,0,   0.3,0,0, 0,0.3,0
--   --       , 0.3,0,0, 0.3,0.3,0, 0,0.3,0
--   --       ] :: S.Vector Float

--   -- vertexBuffer <- allocRef $ glGenBuffers 1
--   -- glBindBuffer GL_ARRAY_BUFFER vertexBuffer
--   -- withStorable vertexBufferData $ \n ptr ->
--   --   glBufferData GL_ARRAY_BUFFER n ptr GL_STATIC_DRAW

--   -- let uvBufferData = S.fromList
--   --       [ 0,0, 1,0, 0,1
--   --       , 1,0, 1,1, 0,1
--   --       ] :: S.Vector Float

--   -- uvBuffer <- allocRef $ glGenBuffers 1
--   -- glBindBuffer GL_ARRAY_BUFFER uvBuffer
--   -- withStorable uvBufferData $ \n ptr ->
--   --   glBufferData GL_ARRAY_BUFFER n ptr GL_STATIC_DRAW

--   -- -- vertcies of
--   -- glEnableVertexAttribArray 0
--   -- glBindBuffer GL_ARRAY_BUFFER vertexBuffer
--   -- glVertexAttribPointer
--   --   0        -- attribute
--   --   3        -- size
--   --   GL_FLOAT -- type
--   --   GL_FALSE -- normalized?
--   --   0        -- stride
--   --   nullPtr  -- array buffer offset

--   -- glEnableVertexAttribArray 1
--   -- glBindBuffer GL_ARRAY_BUFFER uvBuffer
--   -- glVertexAttribPointer
--   --   1         -- attribute
--   --   2         -- size
--   --   GL_FLOAT  -- type
--   --   GL_FALSE  -- normalized?
--   --   0         -- stride
--   --   nullPtr   -- array buffer offset

--   -- Unbind buffer
--   glBindBuffer GL_ARRAY_BUFFER 0

--   vao2 <- allocRef $ glGenVertexArrays 1
--   glBindVertexArray vao2
--   ftBuffer <- initFTGL

--   putStrLn "printing the buffer"
--   printBuffer ftBuffer

--   let kernel = do
--         _events <- SDL.pumpEvents

--         -- tw <- SDL.getTicks
--         -- let t = fromIntegral tw * 1.0e-3
--         -- with (transpose $ mkTransformationMat identity (0.3 *^ V3 (sin t) (cos t) 0):: M44 Float) $
--         --   glUniformMatrix4fv mvpLoc 1 GL_FALSE . castPtr

--         -- glUseProgram programID
--         -- glBindVertexArray vao
--         display ftBuffer
--         draw window
--         -- glBindVertexArray vao2
--         threadDelay 16000

--         kernel

--   draw window
--   kernel

