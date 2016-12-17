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
{-# LANGUAGE ViewPatterns               #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.PGF
-- Copyright   :  (c) 2015 Christopher Chalmers
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- This is an internal module exposeing internals for rendering a
-- diagram. This is for advanced use only. 'Diagrams.Backend.PGF'
-- has enought for general use.
--
module Diagrams.Backend.GL
  -- ( PGF (..)
  -- , Options (..)

  -- , savePGF
  -- , savePGFSurf
  -- , saveOnlinePGF
  -- , saveOnlinePGF'

  -- -- * Lenses
  -- , surface
  -- , sizeSpec
  -- , readable
  -- , standalone

  -- -- * Utilities
  -- , escapeString
  -- )
  where

import qualified Data.Colour                  as Colour
import           Data.Sequence                (Seq)
import qualified Data.Sequence                as Seq
import           System.FilePath              ((</>))

import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Reader         (local)
import           Control.Monad.State
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8        as BS
import qualified Data.ByteString.Lazy         as LB
import           Data.Hashable                (Hashable (..))
import           Data.Maybe                   (fromMaybe)
import           Data.Typeable
import qualified Options.Applicative          as OP
import           System.Directory             (canonicalizePath,
                                               getCurrentDirectory)
import           System.FilePath              (FilePath, takeDirectory,
                                               takeExtension)

import qualified Data.Foldable                as F
import           Diagrams.Backend.Compile
import           Diagrams.Prelude             hiding (clip, local, with, (<~))
import           Diagrams.TwoD.Text

import           Control.Exception
import           Data.FileEmbed

import           Data.Colour.SRGB.Linear      as Linear
import           Data.Traversable
import qualified Data.Vector.Storable         as S
import qualified Data.Vector.Storable.Mutable as M
import           Diagrams.Types               hiding (local)
import           Foreign
import           Foreign.C                    (peekCString, withCString)
import           Geometry.Space
import           Geometry.ThreeD.Types
import           Geometry.TwoD.Types
import           Linear                       (M44, mkTransformationMat,
                                               transpose, (!*!))

import           Geometry.Path

import           Diagrams.ThreeD.Attributes
import           Graphics.GL

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

-- | Infomation needed to draw a 3D line
data LineInfo = LineInfo
  { lineVao         :: !GLuint
  , lineNumSegments :: !Int
  , lineInfoWidth   :: !Float
  , lineInfoColour  :: !(Colour Double)
  , lineInfoModel   :: !(M44 Float)
  } deriving Show

-- | Infomation needed to render a diagram scene
data SceneInfo = SceneInfo
  { _renderBasics :: Seq Basic
  , _renderLines  :: Seq LineInfo
  , _renderLights :: Seq (P3 Double, Colour Double)
  , _sphereInfo   :: !BasicInfo
  , _cubeInfo     :: !BasicInfo
  } -- sphere and cube info don't really need to be here, they should
    -- really be in a reader portion

makeLenses ''SceneInfo

data LineProgram = LineProgram
  { lineProgramId    :: GLuint
  , lineViewId       :: GLint
  , lineProjectionId :: GLint
  , lineModelId      :: GLint
  , lineColourId     :: GLint
  , lineWidthId      :: GLint
  }

data RenderInfo = RenderInfo
  { _renderScene        :: SceneInfo
  , _renderBasicProgram :: BasicProgram
  , _renderLineProgram  :: LineProgram
  }

makeLenses ''RenderInfo

type GLRender = GLRenderM ()

-- | Monad used to compute the RenderInfo for rendering a diagram.
newtype GLRenderM a = GLRender (StateT SceneInfo IO a)
  deriving (Functor, Applicative, Monad, MonadState SceneInfo, MonadIO)

instance Semigroup GLRender where
  (<>) = (>>)
instance Monoid GLRender where
  mappend = (>>)
  mempty  = pure ()

runRender :: GLRender -> IO SceneInfo
runRender (GLRender render) = do
  sphereI <- initSphere
  cubeI   <- initCube
  let renderInfo0 = SceneInfo mempty mempty mempty sphereI cubeI
  execStateT render renderInfo0

-- | Draw a scene from the RenderInfo with the provided view and
--   projection matrices.
drawScene :: RenderInfo -> M44 Float -> M44 Float -> IO ()
drawScene renderInfo viewMat projectionMat = do
  glUseProgram (programID $ _renderBasicProgram renderInfo)

  let mLight = preview (renderScene.renderLights._head) renderInfo
  let (P lightP, lightC) = fromMaybe (2, white) mLight
  uniform3v (lightPosID $ _renderBasicProgram renderInfo) lightP
  -- uniformColour (lightColourID program) lightC

  F.for_ (view (renderScene.renderBasics) renderInfo) (renderBasic viewMat projectionMat (_renderBasicProgram renderInfo))

  drawLines (_renderLineProgram renderInfo) (_renderLines (_renderScene renderInfo)) projectionMat viewMat

diagramRender :: Diagram V3 -> IO RenderInfo
diagramRender dia = do
  prog     <- initProgram
  lineProg <- lineProgram
  scene    <- runRender (toRender mempty dia)
  return $ RenderInfo scene prog lineProg

initProgram :: IO BasicProgram
initProgram = do
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

toRender :: T3 Double -> Diagram V3 -> GLRender
toRender = foldDia renderPrim renderAnnot

renderAnnot :: Annotation V3 Double -> GLRender -> GLRender
renderAnnot a = id
  -- | Just x <- getAnnot _GroupOpacity a = P.opacityGroup x
  -- | Just p <- getAnnot _Clip         a = clip (F.toList p)
  -- | otherwise                          = id

renderPrim :: T3 Double -> Attributes -> Prim V3 Double -> GLRender
renderPrim t attrs = \case
  Cube_           -> use cubeInfo   >>= addBasicPrim t attrs
  Sphere_         -> use sphereInfo >>= addBasicPrim t attrs
  PointLight_ p c -> renderLight (papply t p) c
  Path_ p         -> renderPath t p attrs
  _               -> mempty

getSC :: Attributes -> Colour Double
getSC = fromMaybe grey . getAttr _SurfaceColor

-- | All basic prims use the same shader.
addBasicPrim :: T3 Double -> Attributes -> BasicInfo -> GLRender
addBasicPrim t attrs info = do
  let colour = getSC attrs
      model  = (\(T m _ v) -> mkTransformationMat m v) t

  let basic = Basic
        { basicInfo = info
        , basicColour     = colour
        , basicModel      = (fmap.fmap) realToFrac model
        }

  renderBasics %= cons basic

renderLight :: P3 Double -> Colour Double -> GLRender
renderLight p c = renderLights %= cons (p,c)

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
  let makePlanes !plane !f = go (12*xMax*yMax*plane) 0 0 where
        g x y z = normalize $ f (2* (fromIntegral (x-xMax`div`2)) / fromIntegral xMax)
                                (2* (fromIntegral (y-yMax`div`2)) / fromIntegral yMax)
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
    seg2points (FLinear _ p) = [p]
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

renderPath :: T3 Double -> Path V3 Double -> Attributes -> GLRender
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

  let width   = fromMaybe 0.1 (getAttr _LineWidth attrs)
      acolour = fromMaybe (opaque black) (getAttr _LineWidth attrs)
      -- alpha colours are not supported
      colour  = acolour `Colour.over` white
      modelMat = eye -- we transform before hand for now

  let info = LineInfo
        { lineVao         = vao
        , lineNumSegments = S.length pointData
        , lineInfoWidth   = width
        , lineInfoColour  = colour
        , lineInfoModel   = modelMat
        }

  renderLines %= cons info

-- | Draw the sequence of line infos using the line program and the
-- current projection and view matrix
drawLines :: LineProgram -> Seq LineInfo -> M44 Float -> M44 Float -> IO ()
drawLines lprog lines p v = do
  glUseProgram (lineProgramId lprog)
  with (transpose v) $ glUniformMatrix4fv (lineViewId lprog) 1 GL_FALSE . castPtr
  with (transpose p) $ glUniformMatrix4fv (lineProjectionId lprog) 1 GL_FALSE . castPtr
  F.for_ lines $ \line -> do

    glBindVertexArray (lineVao line)

    let m = lineInfoModel line
    with (transpose m) $ glUniformMatrix4fv (lineModelId lprog) 1 GL_FALSE . castPtr

    uniformColour (lineColourId lprog) (lineInfoColour line)
    glUniform1f (lineColourId lprog) (lineInfoWidth line)

    glDrawArrays GL_LINES 0 (fromIntegral $ lineNumSegments line)

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

-- | Run a function that puts a value in a pointer by temporarly
--   allocatiting a pointer and reading the value from it.
allocRef :: Storable a => (Ptr a -> IO ()) -> IO a
allocRef f = alloca $ \ptr -> f ptr >> peek ptr
{-# INLINE allocRef #-}

withStorable
  :: forall a b. Storable a
  => S.Vector a -> (GLsizeiptr -> Ptr () -> IO b) -> IO b
withStorable (S.unsafeToForeignPtr0 -> (fptr, an)) f =
  withForeignPtr fptr $ \ptr -> f (fromIntegral $ an * sizeOf (undefined :: a)) (castPtr ptr)
{-# INLINE withStorable #-}

storeableBufferData :: Storable a => GLenum -> S.Vector a -> GLenum -> IO ()
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

