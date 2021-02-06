{-# LANGUAGE BlockArguments #-}

module Lib
  ( launchBar,
  )
where

import Data.Bits (Bits ((.|.)))
import Graphics.X11
  (set_override_redirect, moveResizeWindow, setForeground, fillRectangle, copyArea, createPixmap, Visual, defaultColormapOfScreen, createGC,
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
  (Show(show),  Bounded(maxBound), Semigroup((<>)), Integer,  Bool (False, True),
    IO,
    Integral,
    MVar,
    Monad (return),
    Num((+)),
    String,
    const,
    forever,
    fromIntegral,
    fst,
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
import qualified System.FSNotify as FSN (Event (Modified, Removed), watchDir, withManager)
import System.IO (putStrLn)
import Graphics.X11.Xft (XftFont, XftDraw, withXftColorName, xftFontOpen, xftDrawCreate, xftDrawString)
import AtomName (mkAtom, _CARDINAL, _NET_WM_STRUT, _NET_WM_WINDOW_TYPE, _ATOM, _NET_WM_STRUT_PARTIAL, _NET_WM_WINDOW_TYPE_DOCK)
import Data.Yaml (ParseException, decodeFileEither)
import Config (Config, Bar(Bar))
import qualified Config
import Control.Exception (throwIO)
import Core (runSandbarIO, SandbarIO)
import Control.Monad.Except (join, MonadError(catchError), runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Text.Pretty.Simple (pPrint)
import Data.Either (Either(Left, Right))
import qualified Context
import X11InfoRW (X11InfoRW(X11InfoRW))
import qualified X11InfoRW
import X11InfoR (X11InfoR(X11InfoR))
import qualified X11InfoR
import Control.Monad.State (MonadState(put), gets)
import Control.Monad.Reader (asks)
import Control.Concurrent (forkIO)
import RIO.FilePath (FilePath)
import Control.Concurrent.MVar (newEmptyMVar)
import Context (ContextR(ContextR), ContextRW(ContextRW))

newtype ColorCode = ColorCode String

configDirName :: FilePath
configDirName = ".config/sandbar"

configFileName :: FilePath
configFileName = "config.yaml"

getConfig :: IO (Either ParseException Config.Config)
getConfig = do
  homeDir <- getHomeDirectory
  decodeFileEither $ homeDir </> configDirName </> configFileName

data Event
  = Init
  | FileModified
  deriving (Show)

launchBar :: IO ()
launchBar = do
  eConfig <- getConfig

  eventMVar <- newEmptyMVar :: IO (MVar Event)

  _ <- forkIO $ void $ runExceptT do
    (do
       config <- liftEither eConfig
       x11InfoR <- liftIO getX11InfoR
       x11InfoRW <- liftIO $ getX11InfoRW config x11InfoR
       let contextR = ContextR
             { Context.x11InfoR = x11InfoR
             }
           contextRW = ContextRW
             { Context.config = config
             , Context.x11InfoRW = x11InfoRW
             }
       pPrint contextR
       pPrint contextRW
       liftIO $ runSandbarIO (eventLoop eventMVar) contextR contextRW
     ) `catchError` (\e -> do
       liftIO $ throwIO e
     )

  _ <- forkIO $ FSN.withManager $ \mgr -> do
    homeDir <- getHomeDirectory
    _ <- FSN.watchDir mgr (homeDir </> configDirName) (const True) $ \e -> do
      case e of
        (FSN.Modified path _ _) -> do
          putStrLn $ path <> " has been modified."
          putMVar eventMVar FileModified
        (FSN.Removed path _ _) -> do
          putStrLn $ path <> " has been removed."
        _ -> return ()
    forever $ threadDelay maxBound

  putMVar eventMVar Init

  forever $ threadDelay maxBound

eventLoop :: MVar Event -> SandbarIO ()
eventLoop eventMVar = do
  -- Wait for the next event
  event <- takeMVar eventMVar

  liftIO . putStrLn $ "eventLoop: " <> show event
  case event of
    Init -> do
      drawSandbar
    FileModified -> do
      eContext <- getContextRW
      pPrint eContext
      case eContext of
        Left _ -> return ()
        Right context -> do
          x11InfoR <- asks Context.x11InfoR
          x11InfoRW_old <- gets Context.x11InfoRW
          let display = X11InfoR.display x11InfoR
              window = X11InfoRW.window x11InfoRW_old
          liftIO $ destroyWindow display window
          put context
          drawSandbar

  eventLoop eventMVar

getContextRW :: SandbarIO (Either ParseException ContextRW)
getContextRW = do
  x11InfoR <- asks Context.x11InfoR
  eConfig <- liftIO getConfig
  runExceptT $ do
    config <- liftEither eConfig
    x11InfoRW_new <- liftIO $ getX11InfoRW config x11InfoR
    return $ ContextRW { Context.config = config, Context.x11InfoRW = x11InfoRW_new }

drawSandbar :: SandbarIO ()
drawSandbar = do
  config <- gets Context.config
  x11InfoRW <- gets Context.x11InfoRW
  x11InfoR <- asks Context.x11InfoR

  let
    bar = Config.bar config
    cx = Config.x_pos bar
    cy = Config.y_pos bar
    cw = Config.width bar
    ch = Config.height bar
    background_color = Config.background_color bar
    font = Config.font bar
    font_color = Config.font_color bar

  let
    disp = X11InfoR.display x11InfoR
    scr = X11InfoR.screen x11InfoR
    win = X11InfoRW.window x11InfoRW
    colormap = X11InfoRW.colormap x11InfoRW
    gc = X11InfoRW.gc x11InfoRW
    pixmap = X11InfoRW.pixmap x11InfoRW
    visual = X11InfoRW.visual x11InfoRW

  let colorc = ColorCode background_color

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
    xftFont <- xftFontOpen disp scr font
    xftDraw <- xftDrawCreate disp pixmap visual colormap
    xftDrawStringWithColorName disp visual colormap xftDraw (ColorCode font_color) xftFont 100 16 "Hiii"

    -- Copy text pixmap to win
    copyArea disp pixmap win gc cx cy cw ch 0 0

    sync disp False

getX11InfoR :: IO X11InfoR
getX11InfoR = do
  display <- openDisplay ""

  let screenNumber = defaultScreen display

  rootw <- Graphics.X11.rootWindow display screenNumber

  return X11InfoR
    { X11InfoR.display = display
    , X11InfoR.screen = defaultScreenOfDisplay display
    , X11InfoR.screenNumber = screenNumber
    , X11InfoR.rootWindow = rootw
    }

getX11InfoRW :: Config -> X11InfoR -> IO X11InfoRW
getX11InfoRW config x11InfoR = do
  let disp = X11InfoR.display x11InfoR
      scr = X11InfoR.screen x11InfoR
      rootw = X11InfoR.rootWindow x11InfoR
      colormap = defaultColormapOfScreen scr
      visual  = defaultVisualOfScreen scr

      bar = Config.bar config
      cx = Config.x_pos bar
      cy = Config.y_pos bar
      cw = Config.width bar
      ch = Config.height bar

      colorc = ColorCode "#F0F0F0"

  -- Create a bar
  win <- mkUnmanagedWindow disp scr rootw cx cy cw ch colorc

  pixmap <- createPixmap disp win (fi cx + cw) (fi cy + ch) (defaultDepthOfScreen scr)

  -- Create GC
  gc <- createGC disp win

  -- Create GC to reset the window
  gc_clr <- createGC disp win

  return $ X11InfoRW
    { X11InfoRW.colormap = colormap
    , X11InfoRW.visual = visual
    , X11InfoRW.window = win
    , X11InfoRW.pixmap = pixmap
    , X11InfoRW.gc = gc
    , X11InfoRW.gc_clr = gc_clr
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
  threadDelay maxBound
  bgColor' <- initColor disp colormap (ColorCode "#FF0000")
  setForeground disp gc bgColor'
  fillRectangle disp pixmap gc cx cy 800 20
  bgColor'' <- initColor disp colormap (ColorCode "#FF00FF")
  setForeground disp gc bgColor''
  fillRectangle disp pixmap gc 900 10 80 10
  copyArea disp pixmap win gc cx cy cw ch 0 0

  sync disp False


  -- Reset window with pixmap
  threadDelay maxBound
  fillRectangle disp pixmap gc_clr cx cy cw ch
  copyArea disp pixmap win gc cx cy cw ch 0 0

  sync disp False


  threadDelay maxBound
  setForeground disp gc bgColor''
  fillRectangle disp pixmap gc 900 10 80 10
  copyArea disp pixmap win gc cx cy cw ch 0 0

  sync disp False

  threadDelay maxBound

  destroyWindow disp win
  sync disp False
  threadDelay maxBound

  win1 <- mkUnmanagedWindow disp scr rootw cx cy cw ch colorc
  setProperties disp win1
  setStruts (Rectangle cx cy cw ch) disp win1
  moveResizeWindow disp win1 cx cy cw ch
  mapWindow disp win1
  sync disp False

  -- xftFontClose disp xftFont1
  -- xftDrawDestroy xftDraw1

xftDrawStringWithColorName :: Display -> Visual -> Colormap -> XftDraw -> ColorCode -> XftFont -> Integer -> Integer -> String -> IO ()
xftDrawStringWithColorName disp visual colormap xftDraw (ColorCode xftColor) xftFont x y str = withXftColorName disp visual colormap xftColor (\c -> xftDrawString xftDraw c xftFont x y str)

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
  setTextProperty dpy win "sandbar" wM_NAME
  wtype <- mkAtom dpy _NET_WM_WINDOW_TYPE
  dock <- mkAtom dpy _NET_WM_WINDOW_TYPE_DOCK
  changeProperty32 dpy win wtype atom propModeReplace [fi dock]

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
