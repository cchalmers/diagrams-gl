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
{-# LANGUAGE ViewPatterns               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.GL.Util
-- Copyright   :  (c) 2017 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- Utilites for the diagrams-gl backend.
--
module Diagrams.Backend.GL.Util
  (
    SceneView (..)

    -- * Program helpers
  , newProgram
  , compileShader

  , ProgramError (..)
  , loadShaders
  , loadGeomShaders

  -- * General GL helpers
  , allocRef
  , withStorable
  , storeableBufferData

  , uniform3v
  , uniformColour
  )
  where


import           Control.Monad           (when)
import           Control.Monad.IO.Class
import           Data.Typeable

import           Control.Exception
import qualified Data.ByteString         as BS
import           Diagrams.Prelude        hiding (clip, local, with, (<~), e)
import           Foreign.C               (peekCString)

import           Data.Colour.SRGB.Linear as Linear
import           Data.Traversable
import qualified Data.Vector.Storable    as S
import           Foreign
-- import           Geometry.ThreeD.Types

import           Graphics.GL
import Linear (V2, M44)

-- | Matrices used to draw a scene.
data SceneView = SceneView
  { windowSize :: !(V2 Int)
  , viewMatrix :: !(M44 Float)
  , projMatrix :: !(M44 Float)
  }

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

-- | Run a function that puts a value in a pointer by temporarily
--   allocating a pointer and reading the value from it.
allocRef :: Storable a => (Ptr a -> IO ()) -> IO a
allocRef f = alloca $ \ptr -> f ptr >> peek ptr
{-# INLINE allocRef #-}

-- | Access to the size (in bytes) and ptr of a storable vector. The Ptr
--   should only be used to read data.
withStorable
  :: forall a b. Storable a
  => S.Vector a -> (GLsizeiptr -> Ptr () -> IO b) -> IO b
withStorable (S.unsafeToForeignPtr0 -> (fptr, an)) f =
  withForeignPtr fptr $ \ptr -> f (fromIntegral $ an * sizeOf (undefined :: a)) (castPtr ptr)
{-# INLINE withStorable #-}

-- | Run 'glBufferData' on a storable vector.
storeableBufferData
  :: Storable a
  => GLenum     -- ^ Buffer type
  -> S.Vector a -- ^ buffer data
  -> GLenum     -- ^ buffer hint
  -> IO ()
storeableBufferData buffType buffData buffHint =
  withStorable buffData $ \n ptr ->
    glBufferData buffType n ptr buffHint

uniform3v :: MonadIO m => GLint -> V3 Double -> m ()
uniform3v i (fmap realToFrac -> V3 x y z) = glUniform3f i x y z

uniformColour :: MonadIO m => GLint -> Colour Double -> m ()
uniformColour i c = glUniform3f i r g b where
  RGB r g b = realToFrac <$> Linear.toRGB c

-- Programs ------------------------------------------------------------

-- -- | Compile the code for a vertex shader and a fragment shader, then link
-- -- them into a new program. If the compiler or linker fails it will throw
-- -- a ProgramError.
-- newProgram
--   :: BS.ByteString -- ^ vertex shader source code
--   -> BS.ByteString -- ^ fragment shader source code
--   -> IO GLuint
-- newProgram vcode fcode = do
--   vertexShaderId   <- compileShader vcode GL_VERTEX_SHADER
--   fragmentShaderId <- compileShader fcode GL_FRAGMENT_SHADER
--   programId        <- glCreateProgram
--   glAttachShader programId vertexShaderId
--   glAttachShader programId fragmentShaderId
--   glLinkProgram programId
--   result <- allocRef $ glGetProgramiv programId GL_LINK_STATUS
--   when (result == GL_FALSE) $ do
--     n      <- allocRef $ glGetProgramiv programId GL_INFO_LOG_LENGTH
--     errors <- allocaArray (fromIntegral n) $ \ptr -> do
--       glGetProgramInfoLog programId (fromIntegral n) nullPtr ptr
--       peekCString ptr
--     throwIO (LinkError errors)
--   glDeleteShader vertexShaderId
--   glDeleteShader fragmentShaderId
--   return programId

-- | Compile the code for a vertex shader and a fragment shader, then link
-- them into a new program. If the compiler or linker fails it will throw
-- a ProgramError.
newProgram
  :: BS.ByteString -- ^ vertex shader source code
  -> Maybe BS.ByteString -- ^ geometry shader (optional)
  -> BS.ByteString -- ^ fragment shader source code
  -> IO GLuint
newProgram vertBS mgeomBS fragBS = do
  vertShaderId  <- compileShader vertBS GL_VERTEX_SHADER
  mgeomShaderId <- for mgeomBS $ \geomBS -> compileShader geomBS GL_GEOMETRY_SHADER
  fragShaderId  <- compileShader fragBS GL_FRAGMENT_SHADER
  programId     <- glCreateProgram
  glAttachShader programId vertShaderId
  mapM_ (glAttachShader programId) mgeomShaderId
  glAttachShader programId fragShaderId
  glLinkProgram programId
  result <- allocRef $ glGetProgramiv programId GL_LINK_STATUS
  when (result == GL_FALSE) $ do
    n      <- allocRef $ glGetProgramiv programId GL_INFO_LOG_LENGTH
    errors <- allocaArray (fromIntegral n) $ \ptr -> do
      glGetProgramInfoLog programId (fromIntegral n) nullPtr ptr
      peekCString ptr
    throwIO (LinkError errors)
  glDeleteShader vertShaderId
  mapM_ glDeleteShader mgeomShaderId
  glDeleteShader fragShaderId
  return programId

compileShader :: BS.ByteString -> GLenum -> IO GLuint
compileShader code shadeType = do
  shaderId <- glCreateShader shadeType

  BS.useAsCString code $ flip with $ \pptr -> do
    glShaderSource shaderId 1 pptr nullPtr
    glCompileShader shaderId

  result <- with GL_FALSE $ \ptr ->
    glGetShaderiv shaderId GL_COMPILE_STATUS ptr >> peek ptr
  when (result == GL_FALSE) $ do
    n <- allocRef $ glGetShaderiv shaderId GL_INFO_LOG_LENGTH
    errors <- allocaArray (fromIntegral n) $ \ptr -> do
      glGetShaderInfoLog shaderId (fromIntegral n) nullPtr ptr
      peekCString ptr
    case shadeType of
      GL_VERTEX_SHADER   -> throwIO (VertexShaderError errors)
      GL_FRAGMENT_SHADER -> throwIO (FragmentShaderError errors)
      GL_GEOMETRY_SHADER -> throwIO (FragmentShaderError errors)
      _                  -> error $ "unknown shader type\n" ++ errors
  return shaderId

data ProgramError
  = VertexShaderError String
  | FragmentShaderError String
  | GeometryShaderError String
  | LinkError String
  deriving Typeable

instance Show ProgramError where
  show (VertexShaderError e)   = "VertexShaderError:\n" ++ e
  show (FragmentShaderError e) = "FragmentShaderError:\n" ++ e
  show (GeometryShaderError e) = "GeometryShaderError:\n" ++ e
  show (LinkError e)           = "LinkError:\n" ++ e

instance Exception ProgramError where
  -- displayException (VertexShaderError err) = "VertexShaderError:\n" ++ err
  -- displayException (FragmentShaderError err) = "FragmentShaderError:\n" ++ err
  -- displayException (LinkError err) = "LinkError:\n" ++ err

loadShaders :: FilePath -> FilePath -> IO GLenum
loadShaders vertPath fragPath = do
  vert <- BS.readFile vertPath
  frag <- BS.readFile fragPath
  newProgram vert Nothing frag

loadGeomShaders :: FilePath -> FilePath -> FilePath -> IO GLenum
loadGeomShaders vertPath geomPath fragPath = do
  vert <- BS.readFile vertPath
  geom <- BS.readFile geomPath
  frag <- BS.readFile fragPath
  newProgram vert (Just geom) frag



