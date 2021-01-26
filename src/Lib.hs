{-# LANGUAGE BlockArguments #-}

module Lib
  ( launchBar,
  )
where

import Data.Bits (Bits ((.|.)))
import Graphics.X11
  (setForeground, fillRectangle, copyArea, createPixmap, Visual, defaultColormapOfScreen, createGC,  Atom,
    Color (color_pixel),
    Colormap,
    Dimension,
    Pixel,
    Position,
    Rectangle (Rectangle),
    Screen,
    Window,
    allocNamedColor,
    allocaSetWindowAttributes,
    cWBackPixel,
    cWOverrideRedirect,
    createWindow,
    defaultColormap,
    defaultDepthOfScreen,
    defaultScreen,
    defaultScreenOfDisplay,
    defaultVisualOfScreen,
    destroyWindow,
    inputOutput,
    internAtom,
    mapWindow,
    openDisplay,
    rootWindow,
    setTextProperty,
    set_background_pixel,
    set_override_redirect,
    sync,
    wM_NAME,
  )
import Graphics.X11.Xlib
  (Display,
  )
import Graphics.X11.Xlib.Extras
  ( changeProperty32,
    propModeReplace,
  )
import RIO
  (Integer,  Bool (False, True),
    Eq ((/=)),
    IO,
    Integral,
    MVar,
    Monad (return),
    Num((+)),
    Semigroup ((<>)),
    String,
    const,
    filter,
    forever,
    fromIntegral,
    fst,
    newMVar,
    putMVar,
    take,
    takeMVar,
    threadDelay,
    void,
    ($),
    (.),
    (<$>),
    (=<<),
  )
import RIO.Directory (getHomeDirectory)
import RIO.FilePath ((</>))
import System.FSNotify (Event (Modified, Removed), watchDir, withManager)
import System.IO (putStrLn, readFile)
import Graphics.X11.Xft (XftFont, XftDraw, withXftColorName, xftFontOpen, xftDrawCreate, xftDrawString)

newtype ColorCode = ColorCode String

launchBar :: IO ()
launchBar = do
  disp <- openDisplay ""

  let scrNum = defaultScreen disp
      scr = defaultScreenOfDisplay disp
      colormap = defaultColormapOfScreen scr
      visual  = defaultVisualOfScreen scr

  rootw <- rootWindow disp scrNum

  let cx = 0
      cy = 0
      cw = 1920
      ch = 30

  let colorc = ColorCode "#F0F0F0"

  -- Create a bar
  win <- mkUnmanagedWindow disp scr rootw cx cy cw ch colorc

  -- Create pixmap
  pixmap <- createPixmap disp win cw ch (defaultDepthOfScreen scr)

  -- Create GC
  gc <- createGC disp win

  -- Create GC to reset the window
  gc_clr <- createGC disp win

  -- Set properties
  setProperties disp win
  setStruts (Rectangle cx cy cw ch) disp win

  -- Set background to GC
  bgColor <- initColor disp colormap colorc

  -- Set background color with setForeground (because it's going to be filled by drawing rectangle)
  setForeground disp gc bgColor
  setForeground disp gc_clr bgColor

  mapWindow disp win

  -- Clear pixmap
  fillRectangle disp pixmap gc_clr cx cy cw ch

  -- Set text to pixmap
  xftFont <- xftFontOpen disp scr "Iosevka Nerd Font Mono-10"
  xftDraw <- xftDrawCreate disp pixmap visual colormap
  xftDrawStringWithColorName disp visual colormap xftDraw (ColorCode "#FF0000") xftFont 100 16 "Hiii"

  -- Copy text pixmap to win
  copyArea disp pixmap win gc cx cy cw ch 0 0

  sync disp False


  -- Draw rectangles with pixmap
  threadDelay 1000000
  bgColor' <- initColor disp colormap (ColorCode "#FF0000")
  setForeground disp gc bgColor'
  fillRectangle disp pixmap gc cx cy 800 20
  bgColor'' <- initColor disp colormap (ColorCode "#FF00FF")
  setForeground disp gc bgColor''
  fillRectangle disp pixmap gc 900 10 80 10
  copyArea disp pixmap win gc cx cy cw ch 0 0

  sync disp False


  -- Reset window with pixmap
  threadDelay 1000000
  fillRectangle disp pixmap gc_clr cx cy cw ch
  copyArea disp pixmap win gc cx cy cw ch 0 0

  sync disp False


  threadDelay 1000000
  setForeground disp gc bgColor''
  fillRectangle disp pixmap gc 900 10 80 10
  copyArea disp pixmap win gc cx cy cw ch 0 0

  sync disp False

  winVar <- newMVar win

  withManager $ \mgr -> do
    homeDir <- getHomeDirectory
    putStrLn "Hi"
    void $ watchDir mgr (homeDir </> ".config/plainbar") (const True) $ eventLoop winVar disp scr rootw
    forever $ threadDelay 1000000

  -- xftFontClose disp xftFont1
  -- xftDrawDestroy xftDraw1

xftDrawStringWithColorName :: Display -> Visual -> Colormap -> XftDraw -> ColorCode -> XftFont -> Integer -> Integer -> String -> IO ()
xftDrawStringWithColorName disp visual colormap xftDraw (ColorCode xftColor) xftFont x y str = withXftColorName disp visual colormap xftColor (\c -> xftDrawString xftDraw c xftFont x y str)

eventLoop :: MVar Window -> Display -> Screen -> Window -> Event -> IO ()
eventLoop winVar dpy scr rootw event = do
  case event of
    (Modified path _ _) -> do
      putStrLn $ path <> " has been modified."
      destroyWindow dpy =<< takeMVar winVar
      win' <- do
        let cx = 0
            cy = 80
            cw = 1920
            ch = 20
        config <- filter (/= '\n') <$> readFile path
        let colorc = case config of
              "red" -> ColorCode "#FF0000"
              "green" -> ColorCode "#00FF00"
              "blue" -> ColorCode "#0000FF"
              _ -> ColorCode "#D0D0D0"
        win <- mkUnmanagedWindow dpy scr rootw cx cy cw ch colorc
        setProperties dpy win
        setStruts (Rectangle cx cy cw ch) dpy win
        mapWindow dpy win
        return win
      sync dpy False
      putMVar winVar win'
    (Removed path _ _) -> do
      putStrLn $ path <> " has been removed."
    _ -> return ()

mkUnmanagedWindow ::
  Display ->
  Screen ->
  Window ->
  Position ->
  Position ->
  Dimension ->
  Dimension ->
  ColorCode ->
  IO Window
mkUnmanagedWindow dpy scr rw x y w h colorc = do
  let visual = defaultVisualOfScreen scr
      attrmask = cWOverrideRedirect .|. cWBackPixel
      colormap = defaultColormap dpy (defaultScreen dpy)
  allocaSetWindowAttributes $ \attributes -> do
    set_override_redirect attributes True
    set_background_pixel attributes =<< initColor dpy colormap colorc
    createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr) inputOutput visual attrmask attributes

initColor :: Display -> Colormap -> ColorCode -> IO Pixel
initColor dpy colormap (ColorCode color) = color_pixel . fst <$> allocNamedColor dpy colormap color

getStaticStrutValues :: Num a => a -> a -> a -> a -> [a]
getStaticStrutValues cx cy cw ch =
  [0, 0, top, 0, 0, 0, 0, 0, top_start_x, top_end_x, 0, 0]
  where
    top = cy + ch
    top_start_x = cx
    top_end_x = cw + cx

setStruts :: Rectangle -> Display -> Window -> IO ()
setStruts (Rectangle cx cy cw ch) dpy win = do
  let svs = getStaticStrutValues (fi cx) (fi cy) (fi cw) (fi ch)
  card <- mkAtom dpy _CARDINAL
  pstrut <- mkAtom dpy _NET_WM_STRUT_PARTIAL
  strut <- mkAtom dpy _NET_WM_STRUT
  changeProperty32 dpy win pstrut card propModeReplace svs
  changeProperty32 dpy win strut card propModeReplace (take 4 svs)

setProperties :: Display -> Window -> IO ()
setProperties dpy win = do
  atom <- mkAtom dpy _ATOM
  setTextProperty dpy win "plainbar" wM_NAME
  wtype <- mkAtom dpy _NET_WM_WINDOW_TYPE
  dock <- mkAtom dpy _NET_WM_WINDOW_TYPE_DOCK
  changeProperty32 dpy win wtype atom propModeReplace [fi dock]

newtype AtomName = AtomName String

_NET_WM_STRUT_PARTIAL, _NET_WM_STRUT, _NET_WM_WINDOW_TYPE, _NET_WM_WINDOW_TYPE_DOCK, _ATOM, _CARDINAL :: AtomName
_NET_WM_STRUT_PARTIAL = AtomName "_NET_WM_STRUT_PARTIAL"
_NET_WM_STRUT = AtomName "_NET_WM_STRUT"
_NET_WM_WINDOW_TYPE = AtomName "_NET_WM_WINDOW_TYPE"
_NET_WM_WINDOW_TYPE_DOCK = AtomName "_NET_WM_WINDOW_TYPE_DOCK"
_ATOM = AtomName "ATOM"
_CARDINAL = AtomName "CARDINAL"

mkAtom :: Display -> AtomName -> IO Atom
mkAtom dpy (AtomName name) = internAtom dpy name False

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
