module LineGraphics (
  Point, Vector, Line, Path, Picture, Colour,
  white, black, blue, red, green, orange, magenta, lightgreen, darkblue,
  drawPicture,
) where

  -- Rasterific

import Graphics.Rasterific hiding (Point, Vector, Line, Path, polygon)
import Graphics.Rasterific.Texture

  -- JuicyPixels

import Codec.Picture

type Point   = (Float, Float)
type Vector  = (Float, Float)
type Line    = (Point, Point)
type Path    = [Point]
type Colour  = (Int, Int, Int, Int)

type Picture = [(Colour, Path)]

type Colour  = (Int, Int, Int, Int)

-- red, green, blue, opacity

-- Predefined colours
--

:{
let white, black, blue, red, green, orange, magenta, lightgreen, darkblue :: Colour
    white      = (200,  200, 255, 255)
    black      = (  0,    0,   0, 255)
    blue       = (  0,  110, 255, 255)
    red        = (255,    0,   0, 255)
    green      = (10,  255,  10,  235)
    orange     = (255,  255,  0,  200)
    magenta    = (153,    0, 153, 220)
    lightgreen = ( 27,  230,  34, 255)
    darkblue   = ( 24,   50, 194, 255)
:}

darkblue

-- Render a picture composed of coloured line paths with the specified line width.
--

:{
let drawPicture :: Float -> Picture -> Image PixelRGBA8
    drawPicture linewidth picture
      = renderDrawing  800 800 (toColour black) $
          mapM_ (\(col, path) -> withTexture (uniformTexture $ toColour col) (drawPath path)) picture
      where
        drawPath points    = stroke linewidth  JoinRound (CapRound, CapStraight 0) $
                               polyline (map (\(x, y) -> V2 x y) points)
        toColour (a,b,c,d) = PixelRGBA8 (fromIntegral a) (fromIntegral b) (fromIntegral c) (fromIntegral d)
:}

:{
let house :: Path
    house = [(300, 750), (300, 450), (270, 450), (500, 200),
             (730, 450), (700, 450), (700, 750)]
:}


:{
let door :: Path
    door = [(420, 750), (420, 550), (580, 550), (580, 750)]
:}

:{
let p1 :: Picture
    p1 = [(lightgreen, house)]
:}

drawPicture 2.0 p1


encodePng housepng (drawPicture 2.0 [(lightgreen, house), (red, door)])

writePng FILENAME (drawPicture LINEWIDTH PICTURE)


