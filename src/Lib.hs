{-# LANGUAGE BlockArguments #-}

module Lib
  ( launchBar,
  )
where

import Data.Bits (Bits ((.|.)))
import Graphics.X11
  (moveResizeWindow, setForeground, fillRectangle, copyArea, createPixmap, Visual, defaultColormapOfScreen, createGC,
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
    mapWindow,
    openDisplay,
    setTextProperty,
    set_background_pixel,
    sync,
    wM_NAME,
  )
import qualified Graphics.X11 ( rootWindow )
import Graphics.X11.Xlib
  (Display,
  )
import Graphics.X11.Xlib.Extras
  (changeProperty32,
    propModeReplace,
  )
import RIO
  (Semigroup((<>)), Integer,  Bool (False, True),
    Eq ((/=)),
    IO,
    Integral,
    MVar,
    Monad (return),
    Num((+)),
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
    (-),
    (.),
    (<$>),
    (=<<),
  )
import RIO.Directory (getHomeDirectory)
import RIO.FilePath ((</>))
import System.FSNotify (Event (Modified, Removed), watchDir, withManager)
import System.IO (putStrLn, readFile)
import Graphics.X11.Xft (XftFont, XftDraw, withXftColorName, xftFontOpen, xftDrawCreate, xftDrawString)
import AtomName (mkAtom, _CARDINAL, _NET_WM_STRUT, _NET_WM_WINDOW_TYPE, _ATOM, _NET_WM_STRUT_PARTIAL, _NET_WM_WINDOW_TYPE_DOCK)
import Data.Yaml (ParseException, decodeFileEither)
import Config (Config(Config), Bar(Bar))
import qualified Config
import Control.Exception (throwIO)
import Core (runSandbarIO, SandbarIO)
import Control.Monad.Except (MonadError(catchError), runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Text.Pretty.Simple (pPrint)
import Data.Either (Either)
import Context (Context(Context), X11Info(X11Info))
import qualified Context
import Control.Monad.State (gets)
import Control.Monad.Reader (asks)
import Control.Concurrent (forkIO)
import RIO.FilePath (FilePath)

newtype ColorCode = ColorCode String

configDirName :: FilePath
configDirName = ".config/sandbar"

configFileName :: FilePath
configFileName = "config.yaml"

getConfig :: IO (Either ParseException Config.Config)
getConfig = do
  homeDir <- getHomeDirectory
  decodeFileEither $ homeDir </> configDirName </> configFileName

launchBar :: IO ()
launchBar = do
  eConfig <- getConfig

  void $ runExceptT do
    (do
       config <- liftEither eConfig
       x11Info <- liftIO $ getX11Info config
       let context = Context
             { Context.config = config
             , Context.x11Info = x11Info
             }
       pPrint context
       liftIO $ runSandbarIO launchFirstbar context
     ) `catchError` (\e -> do
       liftIO $ throwIO e
     )

  withManager $ \mgr -> do
    homeDir <- getHomeDirectory
    void $ watchDir mgr (homeDir </> configDirName) (const True) $ eventLoop winVar disp scr rootw
    forever $ threadDelay 1000000

  forever do
    threadDelay 1000000
    return ()

launchFirstbar :: SandbarIO ()
launchFirstbar = do
  config <- asks Context.config
  x11Info <- asks Context.x11Info

  let
    bar = Config.bar config
    cx = Config.x_pos bar
    cy = Config.y_pos bar
    cw = Config.width bar
    ch = Config.height bar

  let
    disp = Context.display x11Info
    win = Context.window x11Info
    colormap = Context.colormap x11Info
    gc = Context.gc x11Info
    scr = Context.screen x11Info
    pixmap = Context.pixmap x11Info
    visual = Context.visual x11Info

  let colorc = ColorCode "#D00000"

  liftIO do
    -- Set properties
    setProperties disp win
    setStruts (Rectangle cx cy cw ch) disp win

    -- Set background to GC
    bgColor <- initColor disp colormap colorc

    setForeground disp gc bgColor

    -- Clear pixmap
    fillRectangle disp pixmap gc cx cy cw ch

    mapWindow disp win

    -- Set text to pixmap
    xftFont <- xftFontOpen disp scr "Iosevka Nerd Font Mono-10"
    xftDraw <- xftDrawCreate disp pixmap visual colormap
    xftDrawStringWithColorName disp visual colormap xftDraw (ColorCode "#0000FF") xftFont 100 16 "Hiii"

    -- Copy text pixmap to win
    copyArea disp pixmap win gc cx cy cw ch 0 0

    sync disp False

getX11Info :: Config -> IO X11Info
getX11Info config = do
  disp <- openDisplay ""

  let scrNum = defaultScreen disp
      scr = defaultScreenOfDisplay disp
      colormap = defaultColormapOfScreen scr
      visual  = defaultVisualOfScreen scr

      bar = Config.bar config
      cx = Config.x_pos bar
      cy = Config.y_pos bar
      cw = Config.width bar
      ch = Config.height bar

      colorc = ColorCode "#F0F0F0"

  rootw <- Graphics.X11.rootWindow disp scrNum

  -- Create a bar
  win <- mkUnmanagedWindow disp scr rootw cx cy cw ch colorc

  pixmap <- createPixmap disp win (fi cx + cw) (fi cy + ch) (defaultDepthOfScreen scr)

  -- Create GC
  gc <- createGC disp win

  -- Create GC to reset the window
  gc_clr <- createGC disp win

  return $ X11Info
    { Context.display = disp
    , Context.screenNumber = scrNum
    , Context.rootWindow = rootw
    , Context.colormap = colormap
    , Context.visual = visual
    , Context.screen = scr
    , Context.window = win
    , Context.pixmap = pixmap
    , Context.gc = gc
    , Context.gc_clr = gc_clr
    }

launchBar' :: Config -> IO ()
launchBar' config' = do
  disp <- openDisplay ""

  let scrNum = defaultScreen disp
      scr = defaultScreenOfDisplay disp
      colormap = defaultColormapOfScreen scr
      visual  = defaultVisualOfScreen scr

  rootw <- Graphics.X11.rootWindow disp scrNum

  let bar' = Config.bar config'
      cx = Config.x_pos bar'
      cy = Config.y_pos bar'
      cw = Config.width bar'
      ch = Config.height bar'

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

  threadDelay 1000000

  destroyWindow disp win
  sync disp False
  threadDelay 1000000

  win1 <- mkUnmanagedWindow disp scr rootw cx cy cw ch colorc
  setProperties disp win1
  setStruts (Rectangle cx cy cw ch) disp win1
  moveResizeWindow disp win1 cx cy cw ch
  mapWindow disp win1
  sync disp False

  winVar <- newMVar win

  withManager $ \mgr -> do
    homeDir <- getHomeDirectory
    putStrLn "Hi"
    void $ watchDir mgr (homeDir </> ".config/sandbar") (const True) $ eventLoop winVar disp scr rootw
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
  setTextProperty dpy win "sandbar" wM_NAME
  wtype <- mkAtom dpy _NET_WM_WINDOW_TYPE
  dock <- mkAtom dpy _NET_WM_WINDOW_TYPE_DOCK
  changeProperty32 dpy win wtype atom propModeReplace [fi dock]

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
