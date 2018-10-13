{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Diagrams.Backend.GL.2D
-- Copyright   :  (c) 2017 diagrams team (see LICENSE)
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  diagrams-discuss@googlegroups.com
--
-- The 2D version of the GL backend uses nanovg.
--
-- This is only intended to be a temporary solution. NanoVG doesn't
-- support many features of diagrams (clipping, full gradients etc.) and
-- has no support for 3D rendering and also can't be used in WelGL.
-- NanoVG will probably get replaced when we write our own
-- implimentation of 3D planar paths.
--
------------------------------------------------------------------------------

module Diagrams.Backend.GL.TwoD where


-- import           Control.Lens                 hiding (transform, (#))
-- import           Control.Monad.Reader
-- import           Control.Monad.State          (when, MonadState, State, evalState, mapStateT)
-- import qualified Control.Monad.StateStack     as SS
-- import           Control.Monad.Trans          (lift)

-- import           Data.Default.Class
import qualified Data.Foldable                as F
import           Data.List
import           Data.Maybe                   (fromMaybe)
import Data.Typeable
import Control.Monad
-- import qualified Data.Text                    as T
-- import           Data.Tree                    (Tree(Node))
-- import           Data.Typeable                (Typeable)
-- import           Data.Word                    (Word8)

import Foreign.C.Types

import           Diagrams.Attributes
import           Diagrams.Prelude hiding (r2, p3)
import           Diagrams.Types
import           Diagrams.Backend.Compile
-- import           Diagrams.TwoD.Adjust         (adjustDia2D)
-- import           Diagrams.TwoD.Attributes     (splitTextureFills)
-- import           Diagrams.TwoD.Path           (Clip (Clip))
-- import           Diagrams.TwoD.Text

-- import           Diagrams.Core.Compile
-- import           Diagrams.Core.Transform      (matrixHomRep)
-- import           Diagrams.Core.Types          (Annotation (..))

import qualified NanoVG as VG

-- | This data declaration is simply used as a token to distinguish
--   this rendering engine.
-- data NanoVG = NanoVG
--     deriving (Eq, Ord, Read, Show, Typeable)

-- type B = NanoVG

-- type instance V NanoVG = V2
-- type instance N NanoVG = Double

-- data NanoVGState = NanoVGState { _accumStyle :: Style V2 Double
--                                , _csPos :: (Double, Double) }

-- makeLenses ''NanoVGState

-- instance Default NanoVGState where
--   def = NanoVGState { _accumStyle = mempty
--                     , _csPos = (0,0) }

-- newtype RenderM a =
--   RenderM (SS.StateStackT NanoVGState (ReaderT VG.Context IO) a)
--   deriving (Functor,Applicative,Monad,MonadIO,MonadState NanoVGState,SS.MonadStateStack NanoVGState)

-- instance MonadReader VG.Context RenderM where
--   ask  = RenderM $ lift ask
--   local f (RenderM m) = RenderM $ mapStateStackT (local f) m

-- mapStateStackT :: (m (a,(s,[s])) -> n (b,(s,[s]))) -> SS.StateStackT s m a -> SS.StateStackT s n b
-- mapStateStackT f = SS.StateStackT . mapStateT f . SS.unStateStackT

-- runRenderM :: RenderM a -> ReaderT VG.Context IO a
-- runRenderM (RenderM m) = SS.evalStateStackT m def

-- instance Monoid (Render NanoVG V2 Double) where
--   mempty  = C $ return ()
--   (C c1) `mappend` (C c2) = C (c1 >> c2)

newtype NanoRender = NanoRender { runRender :: VG.Context -> IO () }

instance Semigroup NanoRender where
  NanoRender r1 <> NanoRender r2 = NanoRender $ \ctx -> r1 ctx >> r2 ctx

instance Monoid NanoRender where
  mappend = (<>)
  mempty  = NanoRender $ \_ -> return ()

toRender :: T2 Double -> Diagram V2 -> VG.Context -> IO ()
toRender t d = runRender $ foldDia renderPrim renderAnnot t d

renderAnnot :: Annotation V2 Double -> NanoRender -> NanoRender
renderAnnot _a = id
  -- | Just x <- getAnnot _GroupOpacity a = P.opacityGroup x
  -- | Just p <- getAnnot _Clip         a = clip (F.toList p)
  -- | otherwise                          = id

renderPrim :: T2 Double -> Attributes -> Prim V2 Double -> NanoRender
renderPrim t attrs = \case
  Path_ p          -> NanoRender $ \ctx -> renderPath ctx (transform t p) attrs
  _                -> mempty



renderLine :: VG.Context -> P2 CFloat -> Line V2 Double -> IO (P2 CFloat)
renderLine ctx p0@(P (V2 x0 y0)) t = do
  VG.moveTo ctx x0 y0
  foldlMOf (segments.to (fmap realToFrac)) f p0 t
  where
  f p = \case
    Linear v -> do
      let p' @( P (V2 px py)) = p .+^ v
      VG.lineTo ctx px py
      return p'
    Cubic v1 v2 v3 -> do
      let P (V2 px1 py1) = p .+^ v1
          P (V2 px2 py2) = p .+^ v2
          p3@ (P (V2 px3 py3)) = p .+^ v3
      VG.bezierTo ctx px1 py1 px2 py2 px3 py3
      return p3

renderLoop :: VG.Context -> P2 CFloat -> Loop V2 Double -> IO ()
renderLoop ctx p (Loop l c) =
  case fmap realToFrac c of
    LinearClosing      -> renderLine ctx p l >> VG.closePath ctx
    CubicClosing v1 v2 -> do
      p' <- renderLine ctx p l
      let P (V2 px1 py1) = p' .+^ v1
          P (V2 px2 py2) = p' .+^ v2
          P (V2 px0 py0) = p
      VG.bezierTo ctx px1 py1 px2 py2 px0 py0
      VG.closePath ctx

renderTrail :: VG.Context -> Located (Trail V2 Double) -> IO ()
renderTrail ctx (Loc (fmap realToFrac -> p) t) =
  case t of
    OpenTrail l   -> void $ renderLine ctx p l
    ClosedTrail l -> renderLoop ctx p l

renderPath :: VG.Context -> Path V2 Double -> Attributes -> IO ()
renderPath ctx trails attrs = do
  VG.beginPath ctx
  forOf_ (_Wrapped.folded) trails $ renderTrail ctx

  let attr :: Typeable b => Getting a b a -> a -> a
      attr g d = fromMaybe d $ getAttr g attrs

  let o = attr _Opacity 1

  -- stroking
  let lWidth   = attr _LineWidth      0.5 :: Double
      lTexture = attr _LineTexture    (toTexture black)
      lJoin    = attr _LineJoin       with
      lCap     = attr _LineCap        with
      mlMLimit = getAttr _LineMiterLimit attrs :: Maybe Double
      lo       = attr _StrokeOpacity  1 :: Double
  when (lWidth > 0) $ do
    F.for_ mlMLimit $ VG.miterLimit ctx . realToFrac
    case lTexture of
      SC c -> VG.strokeColor ctx (vgColour c (o*lo))
      LG _g -> return ()
      RG _g -> return ()
    VG.lineJoin ctx (vgJoin lJoin)
    VG.lineCap ctx (vgCap lCap)
    VG.strokeWidth ctx (realToFrac lWidth)
    VG.stroke ctx

  -- filling
  let mfTexture = getAttr _FillTexture attrs
      fo        = attr _FillOpacity 1
  F.for_ mfTexture $ \fTexture -> do
    case fTexture of
      SC c -> VG.fillColor ctx (vgColour c (o*fo))
      LG _g -> return () -- PAINT
      RG _g -> return () -- PAINT
    VG.fill ctx

-- clip :: RenderM ()
-- clip = error "clipping is not supported"

-- byteRange :: Double -> Word8
-- byteRange d = floor (d * 255)

-- data ColorOrPaint = Color VG.Color | Paint VG.Paint

-- texture :: Texture Double -> Double -> RenderM ColorOrPaint
-- texture (SC (SomeColor c)) o = pure $ Color (showColorJS c o)

-- texture (LG g) o =
--   do cxt <- ask
--      case sortBy (comparing (view stopFraction))
--                  (g ^. lGradStops) of
--        [GradientStop c0 0,GradientStop c1 1] ->
--          liftIO $
--          Paint <$>
--          VG.linearGradient cxt
--                            x0
--                            y0
--                            x1
--                            y1
--                            (showColorJS c0 o)
--                            (showColorJS c1 o)
--        _ -> error "Only two color stops are supported" -- TODO throw an exception
--   where
--         (x0,y0) =
--           over both realToFrac $
--           unp2 $
--           transform (g ^. lGradTrans)
--                     (g ^. lGradStart)
--         (x1,y1) =
--           over both realToFrac $
--           unp2 $
--           transform (g ^. lGradTrans)
--                     (g ^. lGradEnd)
--         stops =
--           map (\s ->
--                  (s ^. stopFraction
--                  ,showColorJS (s ^. stopColor)
--                               1))
--               (g ^. lGradStops)

-- texture (RG g) o =
--   do cxt <- ask
--      when ((x0,y0) /= (x1,y1)) $
--        error "Inner and outer circle center have two be the same"
--      case sortBy (comparing (view stopFraction))
--                  (g ^. rGradStops) of
--        [GradientStop c0 0,GradientStop c1 1] ->
--          liftIO $
--          Paint <$>
--          VG.radialGradient cxt
--                            x0
--                            y0
--                            r0
--                            r1
--                            (showColorJS c0 o)
--                            (showColorJS c1 o)
--        _ -> error "Only two color stops are supported" -- TODO throw an exception
--   where (r0,r1) =
--           over both realToFrac $ (s * g ^. rGradRadius0,s * g ^. rGradRadius1)
--         (x0,y0) =
--           over both realToFrac $
--           unp2 $
--           transform (g ^. rGradTrans)
--                     (g ^. rGradCenter0)
--         (x1,y1) =
--           over both realToFrac $
--           unp2 $
--           transform (g ^. rGradTrans)
--                     (g ^. rGradCenter1)
--         s = avgScale $ g ^. rGradTrans

vgColour :: Color c => c -> Double  -> VG.Color
vgColour c o =
    VG.rgbaf (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac (a*o))
  where (r,g,b,a) = colorToSRGBA . toAlphaColour $ c

-- canvasTransform :: T2 Double -> RenderM ()
-- canvasTransform tr = ask >>= \cxt ->
--                          liftIO $ VG.transform cxt
--                                                (realToFrac ax) (realToFrac ay)
--                                                (realToFrac bx) (realToFrac by)
--                                                (realToFrac tx) (realToFrac ty)
--     where
--       [[ax, ay], [bx, by], [tx, ty]] = matrixHomRep tr

-- strokeTexture :: Texture Double -> Double -> RenderM ()
-- strokeTexture txt o = do
--   cxt <- ask
--   colorOrPaint <- texture txt o
--   case colorOrPaint of
--     Color c -> liftIO $ VG.strokeColor cxt c
--     Paint p -> liftIO $ VG.strokePaint cxt p

-- fillTexture :: Texture Double -> Double -> RenderM ()
-- fillTexture txt o = do
--   cxt <- ask
--   colorOrPaint <- texture txt o
--   case colorOrPaint of
--     Color c -> liftIO $ VG.fillColor cxt c
--     Paint p -> liftIO $ VG.fillPaint cxt p

vgCap :: LineCap -> VG.LineCap
vgCap LineCapRound  = VG.Round
vgCap LineCapSquare = VG.Square
vgCap _             = VG.Butt

vgJoin :: LineJoin -> VG.LineCap
vgJoin LineJoinRound = VG.Round
vgJoin LineJoinBevel = VG.Bevel
vgJoin _             = VG.Miter


-- instance Renderable (Text Double) NanoVG where
--   render _ (Text tr al str) = C $ do
--     cxt     <- ask
--     tf      <- fromMaybe "Calibri" <$> getStyleAttrib getFont
--     sz      <- fromMaybe 12 <$> getStyleAttrib getFontSize
--     slant   <- fromMaybe FontSlantNormal <$> getStyleAttrib getFontSlant
--     fw      <- fromMaybe FontWeightNormal <$> getStyleAttrib getFontWeight
--     tx      <- fromMaybe (SC (SomeColor (black :: Colour Double)))
--                <$> getStyleAttrib getFillTexture
--     o       <- fromMaybe 1 <$> getStyleAttrib getOpacity
--     let fSize = avgScale tr * sz
--         vAlign = case al of
--                    BaselineText -> S.singleton VG.AlignBaseline
--                    BoxAlignedText _ h -> case h of
--                      h' | h' <= 0.25 -> S.fromList [VG.AlignBottom,VG.AlignBaseline]
--                      h' | h' >= 0.75 -> S.fromList [VG.AlignTop,VG.AlignBaseline]
--                      _ -> S.fromList [VG.AlignMiddle,VG.AlignBaseline]
--         hAlign = case al of
--                    BaselineText -> S.singleton VG.AlignLeft
--                    BoxAlignedText w _ -> case w of
--                      w' | w' <= 0.25 -> S.singleton VG.AlignLeft
--                      w' | w' >= 0.75 -> S.singleton VG.AlignRight
--                      _ -> S.singleton VG.AlignCenter
--     save
--     -- TODO load correct font
--     liftIO $ VG.textAlign cxt  (hAlign <> vAlign)
--     Just fntId <- liftIO $ VG.createFont cxt "dejavu" (VG.FileName "/Users/christopher/Documents/truetype/DejaVuSerif.ttf")
--     liftIO $ VG.fontFaceId cxt fntId
--     fillTexture tx (realToFrac o)
--     canvasTransform (tr <> reflectionY)
--     liftIO $ VG.text cxt 0 0 (T.pack str)
--     restore

-- instance Renderable (DImage Double External) NanoVG where
--   render _ (DImage path w h tr) = C $ do
--     let ImageRef file = path
--     save
--     canvasTransform (tr <> reflectionY)
--     cxt <- ask
--     -- TODO error handling
--     Just img <- liftIO $ VG.createImage cxt (VG.FileName (T.pack file)) 0
--     imgPattern <- liftIO $ VG.imagePattern cxt 0 0 (fromIntegral w) (fromIntegral h) 0 img 1
--     liftIO $ VG.rect cxt (fromIntegral (-w) /2 ) (fromIntegral (-h) /2) (fromIntegral w) (fromIntegral h)
--     liftIO $ VG.fillPaint cxt imgPattern
--     restore

-- renderNanoVG :: Int -> SizeSpec V2 Double -> QDiagram NanoVG V2 Double Any -> IO ()
-- renderNanoVG port sizeSpec d = VG.blankNanoVG (fromIntegral port) . flip VG.send $ img
--     where
--       img = renderDia NanoVG (NanoVGOptions sizeSpec) d

