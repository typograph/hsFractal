import Codec.Picture
import UI.NCurses
import Control.Monad (forM_, join)
import Control.Monad.IO.Class (liftIO)
import Numeric  (showFFloat)

--import Debug.Trace (traceShowId)

type Point = (Double, Double)

next :: Point -> Point -> Point
next (u,v) (x,y) = (x*x-y*y+u, 2*x*y+v)

mandelbrot :: Point -> Point -> [Point]
mandelbrot z0 p = iterate (next p) z0

julia :: Point -> Point -> [Point]
julia c = iterate (next c)

next3 :: Point -> Point -> Point
next3 (u,v) (x,y) = (x*x*x-3*x*y*y+u, 3*x*x*y - y*y*y +v)

julia3 :: Point -> Point -> [Point]
julia3 c = iterate (next3 c)

mandelbrot3 :: Point -> Point -> [Point]
mandelbrot3 z0 p = iterate (next3 p) z0

data FractalFunction = FractalFunction {
    transform :: Point -> Point -> [Point],
    name :: String
}

mandelbrotF = FractalFunction { transform = mandelbrot, name = "mandelbrot"}
juliaF = FractalFunction { transform = julia, name = "julia"}
julia3F = FractalFunction { transform = julia3, name = "julia^3"}

fairlyClose :: Point -> Bool
fairlyClose (u,v) = (u*u+v*v) < 10

nIter :: Int -> [Point] -> Int
nIter maxiter = length . takeWhile fairlyClose . take maxiter

fIter :: Int -> Int -> Int -> [Point] -> Float
fIter npre ncycle maxiter =  transform . nIter maxiter
    where transform :: Int -> Float
          transform n
                | n == maxiter = -1 * fromIntegral npre
                | n < npre = fromIntegral (n - npre)
                | otherwise =
                    let lg = logBase (fromIntegral ncycle) $ fromIntegral (n - npre + 1)
                    in fromIntegral ncycle * (lg - fromIntegral (floor lg))

fractalAt :: Integral n => n -> n -> ViewerState -> Int -> Int -> (Float -> a) -> (Float -> a) -> Int -> Int -> a
fractalAt w h state npre ncycle fneg fpos i j =
    let (xmin, ymin) = bottomLeft state
        (xmax, ymax) = topRight state
        scale vmin vmax imax i = vmin + fromIntegral i * (vmax-vmin)/ fromIntegral (imax-1)
        fpoints = (function state) (cPoint state) (scale xmin xmax w i, scale ymin ymax h j)
        fi = fIter npre ncycle (depth state) fpoints
    in (if fi < 0 then fneg else fpos) fi
  
-- ASCII rendering

colors = [ColorBlack, ColorBlue, ColorMagenta, ColorRed, ColorYellow, ColorGreen, ColorCyan, ColorBlue]
cpairs = zipWith newColorID (tail colors) colors
chars = map toEnum [32, 9617, 9618, 9619] :: [Char]

drawFractal :: ViewerState -> Update ()
drawFractal state = do
    (h,w) <- windowSize
    sequence_ [ moveCursor j i >> drawGlyph (glyphAt w h (fromEnum i) (fromEnum j)) | i <- [1..w-2], j <- [1..h-2]]
    where ncids = length $ cids state
          nchrs = length chars
          nclrs = (ncids-1) * nchrs
          gci c i = Glyph c [AttributeColor i]
          gNeg fi = gci (chars !! (floor fi + nchrs)) (head . cids $ state)
          gPos fi = gci (chars !! mod (floor fi) nchrs) (cids state !! (1 + div (floor fi) nchrs))
          glyphAt w h = fractalAt w h state nchrs nclrs gNeg gPos

-- HSV rendering

hsv :: Float -> Float -> Float -> PixelRGB8
hsv h s v = 
    let frem x y = x - (y * (fromIntegral $ truncate (x/y)))
        c = s*v
        h' = frem h 360 / 60
        x = c*(1 - abs (h' `frem` 2 - 1))
        (r, g, b) | h' < 1 = (c, x, 0)
                | h' < 2 = (x, c, 0)
                | h' < 3 = (0, c, x)
                | h' < 4 = (0, x, c)
                | h' < 5 = (x, 0, c)
                | h' < 6 = (c, 0, x)
                | otherwise = (0, 0, 0)
        m = v - c
        tune q = round $ (q+m)*255
    in PixelRGB8 (tune r) (tune g) (tune b)
          
renderFractal :: FilePath -> Int -> Int -> ViewerState -> IO ()
renderFractal fname w h state =
    let pNeg fi = PixelRGB8 0 0 . floor $ (1 + fi/100) * 256
        pPos fi = hsv (fi+240) 1 1
        pixelRGBAt = fractalAt w h state 100 360 pNeg pPos
    in writePng fname $ generateImage pixelRGBAt w h

data ViewerState = ViewerState {
    cids :: [ColorID],
    fractalfunc :: FractalFunction,
    depth :: Int,
    ptDelta :: Double,
    cPoint :: Point,
    bottomLeft :: Point,
    topRight :: Point
    }

function = transform . fractalfunc

mapTpl :: (a->b) -> (a,a) -> (b,b)
mapTpl f (x,y) = (f x, f y)

defaultState :: [ColorID] -> Curses ViewerState
defaultState colors = do
    (h, w) <- mapTpl fromIntegral <$> screenSize
    -- I see no terminal-agnostic way of determining the aspect ratio of character cells.
    -- My terminal has 1:2 character cells, so I'll use this (reports range from 1:1 to 1:2.5)
    let scale = max (3 / w) (3 / 2 / h) / 2
    let (cx, cy) = (-0.75, 0.0)
    return $ ViewerState colors mandelbrotF 200 0.1
                         (0,0)
                         (cx-scale*w, cy-2*scale*h)
                         (cx+scale*w, cy+2*scale*h)

switchFlip :: FractalFunction -> ViewerState -> ViewerState
switchFlip f state = state { fractalfunc = f,
                             cPoint = ((xmax + xmin)/2, (ymax + ymin)/2),
                             bottomLeft = (zx - (xmax-xmin)/2, zy - (ymax-ymin)/2),
                             topRight = (zx + (xmax-xmin)/2, zy + (ymax-ymin)/2)
                             }
    where (xmin, ymin) = bottomLeft state
          (xmax, ymax) = topRight state
          (zx, zy) = cPoint state

flipState state = switchFlip (fractalfunc state) state

goDeeper :: ViewerState -> ViewerState
goDeeper state = state { depth = 2 * depth state }

goShallower :: ViewerState -> ViewerState
goShallower state = state { depth = depth state `div` 2}

moveUp :: ViewerState -> ViewerState
moveUp state = state { bottomLeft = (xmin, ymin - (ymax-ymin)/3),
                       topRight = (xmax, ymax - (ymax-ymin)/3) }
    where (xmin, ymin) = bottomLeft state
          (xmax, ymax) = topRight state

moveDown :: ViewerState -> ViewerState
moveDown state = state { bottomLeft = (xmin, ymin + (ymax-ymin)/3),
                         topRight = (xmax, ymax + (ymax-ymin)/3) }
    where (xmin, ymin) = bottomLeft state
          (xmax, ymax) = topRight state
                                      

moveLeft :: ViewerState -> ViewerState
moveLeft state = state { bottomLeft = (xmin - (xmax-xmin)/3, ymin),
                         topRight = (xmax - (xmax-xmin)/3, ymax) }
    where (xmin, ymin) = bottomLeft state
          (xmax, ymax) = topRight state
                         
moveRight :: ViewerState -> ViewerState
moveRight state = state { bottomLeft = (xmin + (xmax-xmin)/3, ymin),
                          topRight = (xmax + (xmax-xmin)/3, ymax) }
    where (xmin, ymin) = bottomLeft state
          (xmax, ymax) = topRight state

zoomIn :: ViewerState -> ViewerState
zoomIn state = state { bottomLeft = (xmin + (xmax-xmin)/4, ymin + (ymax-ymin)/4),
                       topRight = (xmax - (xmax-xmin)/4, ymax - (ymax-ymin)/4) }
    where (xmin, ymin) = bottomLeft state
          (xmax, ymax) = topRight state
    
zoomOut :: ViewerState -> ViewerState
zoomOut state = state { bottomLeft = (xmin - (xmax-xmin)/2, ymin - (ymax-ymin)/2),
                        topRight = (xmax + (xmax-xmin)/2, ymax + (ymax-ymin)/2) }
    where (xmin, ymin) = bottomLeft state
          (xmax, ymax) = topRight state

shiftUp :: ViewerState -> ViewerState
shiftUp state = state { cPoint = (zx, zy + ptDelta state) }
    where (zx, zy) = cPoint state

shiftDown :: ViewerState -> ViewerState
shiftDown state = state { cPoint = (zx, zy - ptDelta state) }
    where (zx, zy) = cPoint state

shiftLeft :: ViewerState -> ViewerState
shiftLeft state = state { cPoint = (zx - ptDelta state, zy) }
    where (zx, zy) = cPoint state

shiftRight :: ViewerState -> ViewerState
shiftRight state = state { cPoint = (zx + ptDelta state, zy) }
    where (zx, zy) = cPoint state

shiftFaster :: ViewerState -> ViewerState
shiftFaster state = state { ptDelta = ptDelta state * 10 }

shiftSlower :: ViewerState -> ViewerState
shiftSlower state = state { ptDelta = ptDelta state / 10 }

main :: IO ()
main = runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    mID <- maxColorID
    cids <- sequence [p i | (p, i) <- zip cpairs [1..mID]]
    draw $ defaultState cids

draw :: Curses ViewerState -> Curses ()
draw mState = do
    win <- defaultWindow
    state <- mState
    updateWindow win $ do
        clear
        drawFractal state
        (h, w) <- windowSize
        let trStr = show $ topRight state
        moveCursor 0 (w - 1 - fromIntegral (length trStr))
        drawString trStr
        moveCursor (h-1) 1
        drawString $ show $ bottomLeft state
        moveCursor 0 1
        drawString $ name (fractalfunc state) ++ " " ++ show (cPoint state) ++ " @" ++ show (depth state)
        moveCursor (h-1) (w-17)
        drawString "Press ? for help"
    render
    continue mState

continue :: Curses ViewerState -> Curses ()
continue mState = do
    win <- defaultWindow
    event <- getEvent win Nothing
    processEvent event mState

saveFigure :: Curses ViewerState -> Curses ()
saveFigure mState = do
    (h,w) <- screenSize
    let (hh, hw) = (5, 20)
    setCursorMode CursorVeryVisible
    statuswin <- newWindow hh hw (div (h-hh) 2) (div (w-hw) 2) 
    updateWindow statuswin $ do
        moveCursor 2 2
        drawString "Rendering ... "
        drawBox Nothing Nothing
    render
    state <- mState
    liftIO $ renderFractal (filename state) (fromIntegral w*10) (fromIntegral h*20)     state
    closeWindow statuswin
    setCursorMode CursorInvisible
    draw mState
    where filename :: ViewerState -> String
          filename state = 
            "snapshot_" ++ name (fractalfunc state)
                        ++ "_@" ++ sp (4,4) (cPoint state)
                        ++ "_" ++ sp (ndigw, ndigh) ((xmin+xmax)/2, (ymin+ymax)/2)
                        ++ "_x" ++ sp (ndigw, ndigh) (fw, fh)
                        ++ ".png"
            where sp (px,py) (x, y) = ("(" ++) . showFFloat (Just px) x $ "," ++ showFFloat (Just py) y ")"
                  (xmin, ymin) = bottomLeft state
                  (xmax, ymax) = topRight state
                  fw = xmax-xmin
                  fh = ymax-ymin
                  ndigw = max 1 $ 1 - floor (logBase 10 fw)
                  ndigh = max 1 $ 1 - floor (logBase 10 fh)

help :: Curses ViewerState -> Curses ()
help state = do
    (h,w) <- screenSize
    let (hh, hw) = (15, 60)
    helpwin <- newWindow hh hw (div (h-hh) 2) (div (w-hw) 2) 
    updateWindow helpwin $ do
        moveCursor 2 0
        drawString
            "      +/-     : Zoom in/out\n\
            \   Arrow keys : Move viewport\n\n\
            \    W/A/S/D   : Move parameter\n\
            \      E/Q     : Increase/decrease parameter move step\n\n\
            \   PgDn/PgUp  : Increase/decrease depth\n\n\
            \       z      : Save snapshot\n\n\
            \      Esc     : Quit "
        drawBox Nothing Nothing
    render
    loop helpwin
    closeWindow helpwin
    draw state
    where loop w = do
            e <- getEvent w Nothing
            case e of
                Just (EventCharacter '?') -> return ()
                Just (EventCharacter 'h') -> return ()
                Just (EventCharacter 'q') -> return ()
                Just (EventCharacter '\27') -> return ()
                _ -> loop w

processEvent :: Maybe Event -> Curses ViewerState -> Curses ()
processEvent (Just (EventCharacter '\27')) = const $ return ()
processEvent (Just (EventCharacter 'h')) = help
processEvent (Just (EventCharacter '?')) = help
processEvent (Just (EventCharacter 'z')) = saveFigure
processEvent (Just (EventCharacter 'r')) = draw . (>>= defaultState) . fmap cids
processEvent (Just (EventCharacter c)) = draw . fmap (char2ViewportAction c)
processEvent (Just (EventSpecialKey k)) = draw . fmap (key2ViewportAction k)
processEvent _ = continue

char2ViewportAction :: Char -> ViewerState -> ViewerState
char2ViewportAction '+' = zoomIn
char2ViewportAction '-' = zoomOut
char2ViewportAction 'w' = shiftUp
char2ViewportAction 'a' = shiftLeft
char2ViewportAction 's' = shiftDown
char2ViewportAction 'd' = shiftRight
char2ViewportAction 'q' = shiftSlower
char2ViewportAction 'e' = shiftFaster
char2ViewportAction 'j' = switchFlip juliaF
char2ViewportAction 'm' = switchFlip mandelbrotF
char2ViewportAction _ = id

key2ViewportAction KeyUpArrow = moveUp
key2ViewportAction KeyDownArrow = moveDown
key2ViewportAction KeyLeftArrow = moveLeft
key2ViewportAction KeyRightArrow = moveRight
key2ViewportAction KeyNextPage = goDeeper
key2ViewportAction KeyPreviousPage = goShallower
key2ViewportAction _ = id
