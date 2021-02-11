{-# LANGUAGE BlockArguments #-}

module Lib where

import Data.Bits (Bits ((.|.)))
import Graphics.X11
  (copyArea, createPixmap, setWindowBackground, set_override_redirect, setForeground, fillRectangle, defaultColormapOfScreen, createGC,
    Color (color_pixel),
    Dimension,
    Pixel,
    Position,
    Screen,
    Window,
    allocNamedColor,
    allocaSetWindowAttributes,
    cWBackPixel,
    cWOverrideRedirect,
    createWindow,
    defaultDepthOfScreen,
    defaultScreen,
    defaultScreenOfDisplay,
    defaultVisualOfScreen,
    destroyWindow,
    inputOutput,
    mapWindow,
    openDisplay,
    setTextProperty,
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
  (Foldable(length), Applicative((<*>)), Eq((==)), Show(show),  Bounded(maxBound), Semigroup((<>)), Integer,  Bool (False, True),
    IO,
    Integral,
    MVar,
    Monad (return),
    Num((*), (+)),
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
  )
import RIO.Directory (getHomeDirectory)
import RIO.FilePath ((</>), FilePath)
import qualified System.FSNotify as FSN (Event (Modified), watchDir, withManager)
import System.IO (putStr, putStrLn)
import Graphics.X11.Xft (xftfont_max_advance_width, XftFont, XftDraw, withXftColorName, xftFontOpen, xftDrawCreate, xftDrawString)
import AtomName (mkAtom, _CARDINAL, _NET_WM_STRUT, _NET_WM_WINDOW_TYPE, _ATOM, _NET_WM_STRUT_PARTIAL, _NET_WM_WINDOW_TYPE_DOCK)
import Data.Yaml (ParseException, decodeFileEither)
import Config (Config)
import qualified Config
import Core (runSandbarIO, SandbarIO)
import Control.Monad.Except (MonadError(catchError))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Text.Pretty.Simple (pPrint)
import Data.Either (Either(Right, Left))
import qualified Context
import X11InfoRW (X11InfoRW(X11InfoRW))
import qualified X11InfoRW
import X11InfoR (X11InfoR(X11InfoR))
import qualified X11InfoR
import Control.Monad.State (modify, MonadState(put), gets)
import Control.Monad.Reader (asks)
import Control.Concurrent (newMVar, forkIO)
import Control.Concurrent.MVar (newEmptyMVar)
import Context (ContextR(ContextR), ContextRW(ContextRW))
import Control.Monad ((=<<), when, forM_)

configDirName :: FilePath
configDirName = ".config/sandbar"

configFileName :: FilePath
configFileName = "config.yaml"

getConfig :: IO (Either ParseException Config)
getConfig = do
  homeDir <- getHomeDirectory
  decodeFileEither $ homeDir </> configDirName </> configFileName

data Action
  = UpdateContext Config
  | Draw
  deriving (Show)

data Event
  = FileModified
  deriving (Show)

mkMVarOperations :: MVar a -> IO (a -> IO (), IO a)
mkMVarOperations mVar = do
  let p = putMVar mVar
      t = takeMVar mVar
  return (p, t)

init :: IO ()
init = do
  (putEvent, takeEvent) <- mkMVarOperations =<< newEmptyMVar

  -- Start the config file (~/.config/sandbar/config.yaml) watcher.
  _ <- forkIO $ watchConfigfile putEvent

  -- Try to read and decode the config file.
  eConfig <- getConfig

  -- Block until decoding the config file completes.
  config <- case eConfig of
    Right c -> return c
    Left e -> do
      putStrLn "Error found. Please fix the config file:"
      pPrint e
      initLoop takeEvent

  (putAction, takeAction) <- mkMVarOperations =<< newMVar Draw

  _ <- forkIO $ eventLoop takeEvent putAction

  x11InfoR <- getX11InfoR
  x11InfoRW <- getX11InfoRW config x11InfoR
  let contextR = ContextR { Context.x11InfoR = x11InfoR }
      contextRW = ContextRW { Context.config = config, Context.x11InfoRW = x11InfoRW }

  _ <- runSandbarIO (launchBar takeAction) contextR contextRW

  return ()

launchBar :: IO Action -> SandbarIO ()
launchBar takeAction = go where
  go = do
    -- Wait for the next action
    action <- liftIO takeAction

    liftIO . putStrLn $ "action: " <> show action
    case action of
      UpdateContext config -> do
        modify \old -> old { Context.config = config }
        context <- getContextRW
        pPrint context
        x11InfoR <- asks Context.x11InfoR
        x11InfoRW_old <- gets Context.x11InfoRW
        let display = X11InfoR.display x11InfoR
            window = X11InfoRW.window x11InfoRW_old
        liftIO $ destroyWindow display window
        put context
      Draw -> do
        drawSandbar
    go

eventLoop :: IO Event -> (Action -> IO ()) -> IO ()
eventLoop takeEvent putAction = go where
  go = do
    -- Wait for the next event
    event <- takeEvent

    putStrLn $ "eventLoop: " <> show event
    case event of
      FileModified -> do
        eConfig <- getConfig
        case eConfig of
          Right config -> do
            putAction (UpdateContext config)
            putAction Draw
          Left e -> pPrint e
        go

watchConfigfile :: (Event -> IO ()) -> IO ()
watchConfigfile putEvent = void $ do
  FSN.withManager $ \mgr -> do
    homeDir <- getHomeDirectory
    _ <- FSN.watchDir mgr (homeDir </> configDirName) (const True) $ \e -> do
      case e of
        (FSN.Modified path _ _) -> do
          when (path == homeDir </> configDirName </> configFileName) do
            putStrLn $ path <> " has been modified."
            putEvent FileModified
        _ -> return ()
    forever $ threadDelay maxBound

initLoop :: IO Event -> IO Config
initLoop takeEvent = go where
  go = do
    -- Wait for the next event
    event <- takeEvent

    putStrLn $ "initLoop: " <> show event
    case event of
      FileModified -> do
        eConfig <- getConfig
        case eConfig of
          Right r -> return r
          Left e -> do
            pPrint e
            go

getContextRW :: SandbarIO ContextRW
getContextRW = do
  x11InfoR <- asks Context.x11InfoR
  config <- gets Context.config
  x11InfoRW_new <- liftIO $ getX11InfoRW config x11InfoR
  return $ ContextRW { Context.config = config, Context.x11InfoRW = x11InfoRW_new }

drawSandbar :: SandbarIO ()
drawSandbar = do
  config <- gets Context.config
  x11InfoRW <- gets Context.x11InfoRW
  x11InfoR <- asks Context.x11InfoR

  let
    bar = Config.bar config
    ch = Config.bar_height bar
    background_color = Config.bar_background_color bar
    texts = Config.text bar

  let
    disp = X11InfoR.display x11InfoR
    scr = X11InfoR.screen x11InfoR
    colormap = X11InfoR.colormap x11InfoR
    win = X11InfoRW.window x11InfoRW
    gc = X11InfoRW.gc x11InfoRW
    visual = X11InfoRW.visual x11InfoRW

  bar_background_color <- initColor background_color

  -- Set properties
  setProperties
  setStruts

  liftIO do
    setWindowBackground disp win bar_background_color
    mapWindow disp win

  -- Set text to window
  forM_ texts \text -> do
    let
      text_value = Config.text_value text
      text_font = Config.text_font text
      text_background_color = Config.text_background_color text
      font_color = Config.text_color text
      font_x_pos = Config.text_x_pos text
      font_y_pos = Config.text_y_pos text
    bgColor <- initColor text_background_color
    (xftDraw, xftFont, pixmapWidth, pixmap) <- liftIO do
      xftFont <- xftFontOpen disp scr text_font
      pixmapWidth <- (*) <$> xftfont_max_advance_width xftFont <*> return (length text_value)
      pixmap <- createPixmap disp win (fi pixmapWidth) ch (defaultDepthOfScreen scr)
      xftDraw <- xftDrawCreate disp pixmap visual colormap
      setForeground disp gc bgColor
      fillRectangle disp pixmap gc 0 0 (fi pixmapWidth) ch
      return (xftDraw, xftFont, pixmapWidth, pixmap)

    xftDrawStringWithColorName xftDraw font_color xftFont 0 font_y_pos text_value

    liftIO do
      -- copyArea Display Pixmap Window GC Pix_x Pix_y (fi pixmapWidth) ch Win_x Win_y
      copyArea disp pixmap win gc 0 0 (fi pixmapWidth) ch (fi font_x_pos) 0

  liftIO do
    sync disp False

getX11InfoR :: IO X11InfoR
getX11InfoR = do
  display <- openDisplay ""

  let
    screenNumber = defaultScreen display
    screen = defaultScreenOfDisplay display
    colormap = defaultColormapOfScreen screen
    defaultColorName = "#606060"

  rootw <- Graphics.X11.rootWindow display screenNumber
  defaultColor <- color_pixel . fst <$> allocNamedColor display colormap defaultColorName

  return X11InfoR
    { X11InfoR.display = display
    , X11InfoR.screen = screen
    , X11InfoR.screenNumber = screenNumber
    , X11InfoR.rootWindow = rootw
    , X11InfoR.colormap = colormap
    , X11InfoR.defaultColorName = defaultColorName
    , X11InfoR.defaultColor = defaultColor
    }

getX11InfoRW :: Config -> X11InfoR -> IO X11InfoRW
getX11InfoRW config x11InfoR = do
  let disp = X11InfoR.display x11InfoR
      scr = X11InfoR.screen x11InfoR
      rootw = X11InfoR.rootWindow x11InfoR
      visual  = defaultVisualOfScreen scr

      bar = Config.bar config
      cx = Config.bar_x_pos bar
      cy = Config.bar_y_pos bar
      cw = Config.bar_width bar
      ch = Config.bar_height bar

  -- Create a bar
  win <- mkUnmanagedWindow disp scr rootw cx cy cw ch

  -- Create GC
  gc <- createGC disp win

  -- Create GC to reset the window
  gc_clr <- createGC disp win

  return $ X11InfoRW
    { X11InfoRW.visual = visual
    , X11InfoRW.window = win
    , X11InfoRW.gc = gc
    , X11InfoRW.gc_clr = gc_clr
    }

xftDrawStringWithColorName :: XftDraw -> String -> XftFont -> Integer -> Integer -> String -> SandbarIO ()
xftDrawStringWithColorName xftDraw color xftFont x y str = do
  x11InfoRW <- gets Context.x11InfoRW
  x11InfoR <- asks Context.x11InfoR
  let
    display = X11InfoR.display x11InfoR
    colormap = X11InfoR.colormap x11InfoR
    defaultColorName = X11InfoR.defaultColorName x11InfoR
    visual = X11InfoRW.visual x11InfoRW
  liftIO do
    ( do
      _ <- allocNamedColor display colormap color -- Throws an error if the color didn't exist
      withXftColorName display visual colormap color (\c -> xftDrawString xftDraw c xftFont x y str)
     ) `catchError` \e -> do
       putStr $ "Error from xftDrawStringWithColorName(" <> color <> "): "
       pPrint e
       withXftColorName display visual colormap defaultColorName (\c -> xftDrawString xftDraw c xftFont x y str)

mkUnmanagedWindow ::
  Display ->
  Screen ->
  Window ->
  Position ->
  Position ->
  Dimension ->
  Dimension ->
  IO Window
mkUnmanagedWindow dpy scr rw x y w h = do
  let visual = defaultVisualOfScreen scr
      attrmask = cWOverrideRedirect .|. cWBackPixel
  allocaSetWindowAttributes $ \attributes -> do
    set_override_redirect attributes True
    createWindow dpy rw x y w h 0 (defaultDepthOfScreen scr) inputOutput visual attrmask attributes

initColor :: String -> SandbarIO Pixel
initColor color = do
  x11InfoR <- asks Context.x11InfoR
  let
    display = X11InfoR.display x11InfoR
    colormap = X11InfoR.colormap x11InfoR
    defaultColor = X11InfoR.defaultColor x11InfoR
  liftIO do
    ( color_pixel . fst <$> allocNamedColor display colormap color
     ) `catchError` \e -> do
       putStr $ "Error from initColor(" <> color <> "): "
       pPrint e
       return defaultColor

getStaticStrutValues :: Num a => a -> a -> a -> a -> [a]
getStaticStrutValues cx cy cw ch =
  [0, 0, top, 0, 0, 0, 0, 0, top_start_x, top_end_x, 0, 0]
  where
    top = cy + ch
    top_start_x = cx
    top_end_x = cw + cx

setStruts :: SandbarIO ()
setStruts = do
  config <- gets Context.config
  x11InfoRW <- gets Context.x11InfoRW
  x11InfoR <- asks Context.x11InfoR
  let
    bar = Config.bar config
    cx = Config.bar_x_pos bar
    cy = Config.bar_y_pos bar
    cw = Config.bar_width bar
    ch = Config.bar_height bar
    svs = getStaticStrutValues (fi cx) (fi cy) (fi cw) (fi ch)
    display = X11InfoR.display x11InfoR
    window = X11InfoRW.window x11InfoRW
  liftIO do
    card <- mkAtom display _CARDINAL
    pstrut <- mkAtom display _NET_WM_STRUT_PARTIAL
    strut <- mkAtom display _NET_WM_STRUT
    changeProperty32 display window pstrut card propModeReplace svs
    changeProperty32 display window strut card propModeReplace (take 4 svs)

setProperties :: SandbarIO ()
setProperties = do
  x11InfoRW <- gets Context.x11InfoRW
  x11InfoR <- asks Context.x11InfoR
  let
    display = X11InfoR.display x11InfoR
    window = X11InfoRW.window x11InfoRW
  liftIO do
    atom <- mkAtom display _ATOM
    setTextProperty display window "sandbar" wM_NAME
    wtype <- mkAtom display _NET_WM_WINDOW_TYPE
    dock <- mkAtom display _NET_WM_WINDOW_TYPE_DOCK
    changeProperty32 display window wtype atom propModeReplace [fi dock]

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
