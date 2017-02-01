{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Diagrams.Backend.GL.Text where

import Geometry.Angle

import Data.FileEmbed

import Foreign.C.String
import Data.Primitive.ByteArray
import Foreign
import qualified Data.Vector.Storable as S
import Letters.Internal
import qualified Data.Colour as C
import           Data.Colour.SRGB.Linear
import Linear
import Data.Foldable
import Graphics.GL
import Data.Foldable as F
import Data.Semigroup
-- import Data.Int
-- import Data.Word
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Control.Lens

import Data.List (sortOn)

import Diagrams.Backend.GL.Util

-- For now text is limited to the simplest case: constant size at some
-- position, rotation and (alpha) colour. Note that even in 3D this text
-- always faces the camera. Text rendered on an arbitrary plane or text
-- that changes size depending on the view point is not yet supported
-- (although I do plan to add support for this later (see below)).

-- Ideas for planar text support:
--   - One way would be to prerender largeish (64x64 or 128x128)
--     distance fields for each glyph. These should give good
--     results as long as they're not stretched to extremes (in which
--     case they'll probably have rounded/wiggly edges). Distance
--     fields are also nice because we can do fancy effects like
--     outline, glow, drop shadow etc.
--   - The more likely way is to just treat text like any other planar
--     path (which is quite a bit job in itself). Planar path support
--     should be good for even extreme viewing angles. Although I don't
--     know if we can acheive fancy effects like we can with

-- Other random things thoughts about text:
--   - Should we fallback to symbol fonts when a symbol in not in the
--     current font.
--   - FreeType says it has support for rendering emoji. It shoudln't be
--     so hard to support those too.

-- | The infomation needed to access the texture for some glyph.
--
--   For now each glyph has its own texture but it's much more efficient
--   to have an atlas containing the glyphs. (Then we can do a single
--   instanced draw for all text in the scene).
data GlyphTexture = GlyphTexture
  { glyphTextureId     :: !GLuint
  , glyphTextureWidth  :: !Int
  , glyphTextureHeight :: !Int
  , glyphTextureLeft   :: !Int
  , glyphTextureTop    :: !Int
  }

-- | A map from a character index to the precomputed 'GlyphTexture' for
--   the corresponding glyph to the 'CharIndex' of a single font.
newtype GlyphMap = GlyphMap (HashMap CharIndex GlyphTexture)
  deriving (Semigroup, Monoid)

gm :: Iso' GlyphMap (HashMap CharIndex GlyphTexture)
gm = coerced

-- | Add any new character in the string to a glyph map. The same font
--   should be used that constructed the glyph map.
-- addCharacters :: String -> GlyphMap -> Face (IO GlyphMap)
-- addCharacters str =

addGlyphs :: FontFace -> GlyphMap -> [CharIndex] -> IO GlyphMap
addGlyphs ff = F.foldlM f where
  f (GlyphMap m) i = do
    GlyphBitmap l t (Bitmap w h p img) <- bitmap ff i
    let ixImage x y = indexByteArray img (y*p + x) :: Word8

    if M.member i m
      then return (GlyphMap m)
      else do
        -- Disable byte-alignment restriction (temporary)
        glPixelStorei GL_UNPACK_ALIGNMENT 1

        texture <- allocaBytes (w*h) $ \ptr -> do
          F.for_ [0..w-1] $ \x ->
            F.for_ [0..h-1] $ \y ->
              pokeElemOff (castPtr ptr) (y*w + x) (ixImage x y)
          textureID <- allocRef $ glGenTextures 1
          glBindTexture GL_TEXTURE_2D textureID
          glTexImage2D
            GL_TEXTURE_2D
            0
            GL_RED
            (fromIntegral w)
            (fromIntegral h)
            0
            GL_RED
            GL_UNSIGNED_BYTE
            ptr
          return textureID

        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP_TO_EDGE
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP_TO_EDGE
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR
        glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR

        let g = GlyphTexture texture w h l t

        return (GlyphMap $ M.insert i g m)

-- | A fixed floating point number which uses 26 bits for the integral
--   part and 6 bits for the fractional part. Each unit represents
--   1/64th of a pixel.
-- newtype Fixed26_6 = Fixed26_6 Int32
type Fixed26_6 = Int32

-- | A FreeType position
type FT_Pos = V2 Fixed26_6

-- $rendering-glyphs
-- The following things need to be set before rendering a glyph
--   - current shader as textShader
--   - "text_pos" (position of text anchor in 3D space) uniform should be set
--   - "text_colour" uniform should be set
--   - glDepthMask should be disabled (the glyphs use transparency)
--   - the 'textOffsetBuffer' needs to be bound
--
-- also note since these are rendered with transparency they need to be
-- rendered in order (back to front) if anything with transparency
-- overlaps (otherwise the text could have a fient outline of the glyphs
-- anti-aliasing).

-- | Render a single glyph at position relative to the text_pos (low
--   level)
--
--   It does so by
--     - updating textOffsetBuffer (should already be bound)
--     - bind glyph texture
--     - issue draw command
renderGlyph
  :: V2 Int    -- ^ size of the window we're rendering to
  -> GlyphTexture -- ^ info for the glyph to render
  -> FT_Pos    -- ^ position relative to the "text_angle" to draw the
               --   glyph (this ignores the current transform)
  -> IO ()
renderGlyph (V2 winW winH) GlyphTexture {..} (V2 i j) = do
  -- The glyph texture dimensions are in pixels. We get them to screen
  -- dimensions by dividing by the window size.
  let xMin = (fromIntegral i/64 + fromIntegral glyphTextureLeft) / fromIntegral winW
      yMin = (fromIntegral j/64 + fromIntegral (glyphTextureTop - glyphTextureHeight)) / fromIntegral winH
      xMax = xMin + fromIntegral glyphTextureWidth  / fromIntegral winW
      yMax = yMin + fromIntegral glyphTextureHeight / fromIntegral winH

      vertexBufferData :: S.Vector (V2 Float)
      vertexBufferData = S.fromList
        [ V2 xMin yMax, V2 xMin yMin, V2 xMax yMin
        , V2 xMin yMax, V2 xMax yMin, V2 xMax yMax
        ]

  glBindTexture GL_TEXTURE_2D glyphTextureId

  withStorable vertexBufferData $ \n ptr ->
    glBufferSubData GL_ARRAY_BUFFER 0 n ptr
  glDrawArrays GL_TRIANGLES 0 6

-- | Render a string of glyphs with an advances. The 'Advances' and
--   'GlyphMap' must be built using the same font. See $rendering-glyphs
--   for prerequisites to call this function.
renderAdvances
  :: V2 Int   -- ^ window size
  -> GlyphMap
  -> Advances
  -> IO ()
renderAdvances winSz (GlyphMap glyphMap) adv =
  flip foldAdvances adv $ \charIx x -> do
    case M.lookup charIx glyphMap of
      Nothing -> error "char index not in char map (probably not initialilsed properly or font mixup)"
      Just gI -> renderGlyph winSz gI (V2 (fromIntegral x) 0)

-- | Infomation needed to draw a single string of text
data TextInfo = TextInfo
  { textAdvances :: !Advances
  , textPosition :: !(V3 Float)
  , textColour   :: !(V4 Float)
  , textAngle    :: !Float
  }

-- | The program diagrams-gl uses to draw lines in 3D space that always
--   face the camera.
data TextProgram = TextProgram
  { textProgramId    :: !GLuint
  , textTextureId    :: !GLint
  , textAnchorId     :: !GLint
  , textColourId     :: !GLint
  , textVao          :: !GLuint
  , textOffsetBuffer :: !GLuint
  , textUvBuffer     :: !GLuint
  }

initTextProgram :: IO TextProgram
initTextProgram = do
  let basicVert = $(embedFile "shaders/text.vert")
      basicFrag = $(embedFile "shaders/text.frag")
  programID <- newProgram basicVert Nothing basicFrag
  glUseProgram programID

  let getLoc nm = withCString nm (glGetUniformLocation programID)

  anchorLoc    <- getLoc "Anchor"
  txtColourLoc <- getLoc "textColour"
  textureLoc   <- getLoc "glyph"

  vao            <- allocRef $ glGenVertexArrays 1
  glBindVertexArray vao

  glUniform1i textureLoc 0

  glyphOffsetBuffer <- allocRef $ glGenBuffers 1
  texCoordBuffer    <- allocRef $ glGenBuffers 1

  -- The offset of the current glyph vertex from the text anchor (in
  -- window coordinates)
  glBindBuffer GL_ARRAY_BUFFER glyphOffsetBuffer
  glBufferData
    GL_ARRAY_BUFFER
    (fromIntegral $ sizeOf(undefined::GLfloat) * 6 * 4)
    nullPtr
    GL_DYNAMIC_DRAW

  glEnableVertexAttribArray 0
  glVertexAttribPointer
    0
    2
    GL_FLOAT
    GL_FALSE
    0
    nullPtr

  -- the texture coordinate attribute
  glBindBuffer GL_ARRAY_BUFFER texCoordBuffer
  let texCoords = S.fromList [ 0,0, 0,1, 1,1 , 0,0, 1,1, 1,0 ] :: S.Vector Float
  withStorable texCoords $ \n ptr ->
    glBufferData GL_ARRAY_BUFFER n ptr GL_STATIC_DRAW

  glEnableVertexAttribArray 1
  glVertexAttribPointer
    1
    2
    GL_FLOAT
    GL_FALSE
    0
    nullPtr

  glBindVertexArray 0

  return $ TextProgram
    { textProgramId    = programID
    , textTextureId    = textureLoc
    , textAnchorId     = anchorLoc
    , textColourId     = txtColourLoc
    , textVao          = vao
    , textOffsetBuffer = glyphOffsetBuffer
    , textUvBuffer     = texCoordBuffer
    }

data TextRenders = TextRenders !GlyphMap ![TextInfo]

-- initTextRenders :: [String] -> TextRenders

mkTextInfo
  :: FontFace
  -> String
  -> V3 Double
  -> C.AlphaColour Double
  -> Angle Double
  -> IO TextInfo
mkTextInfo ff str pos (C.alphaColourConvert -> ac) ang = do
  a  <- advances ff str
  let RGB r g b = toRGB $ ac `C.over` C.black
  return $ TextInfo
    { textAdvances = a
    , textPosition = realToFrac <$> pos
    , textColour   = V4 r g b (C.alphaChannel ac)
    , textAngle    = realToFrac $ ang^.rad
    }


-- mkTextRenders :: [TextInfo] -> (IO TextRenders)
-- mkTextRenders is = do
--   let charIndexes = concatMap (\ti -> advancesCodepoints (textAdvances ti)) is
--   ioGlyphMap <- makeGlyphMap charIndexes
--   return $ do
--     glyphMap <- ioGlyphMap
--     return (TextRenders glyphMap is)

renderTextRenders
  :: Foldable f
  => V2 Int
  -> M44 Float
  -> TextProgram
  -> GlyphMap
  -> f TextInfo
  -> IO ()
renderTextRenders winSz mvp TextProgram {..} glyphMap is = do
  glUseProgram textProgramId
  glBindVertexArray textVao

  -- the offset buffer gets written to when rendering each glyph
  glBindBuffer GL_ARRAY_BUFFER textOffsetBuffer
  glActiveTexture GL_TEXTURE0
  glUniform1i textTextureId 0

  -- Glyphs are rendered using transparency so we can't rely on the
  -- z-buffer to order overlapping text. But we still want to use the
  -- z-buffer so we don't overlap solid objects in the scene (which have
  -- all already been rendered). So we disable the depth mask (so we
  -- don't overwrite the z-buffer) and render the text in order,
  -- depending on the current mvp matrix.

  glDepthMask GL_FALSE

  -- XXX once we support other transparent things this won't work. We'd
  -- have to order ALL thing with transparency and render them in order.
  -- (this makes rendering text using textures for 3D less attractive)
  let getP i = normalizePoint $ mvp !* point (textPosition i)
      withV  = is ^.. folded . to (\i -> (getP i, i))
      sorted = sortOn (negate . view _z . fst) withV

  for_ sorted $ \(V3 x y z, info) -> do
    let V4 r g b a = textColour info
    glUniform4f textColourId r g b a
    glUniform3f textAnchorId x y z
    renderAdvances winSz glyphMap (textAdvances info)

