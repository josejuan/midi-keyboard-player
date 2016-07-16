{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Monoid
import Data.Maybe
import Options.Generic
import Sound.PortMidi
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Musica.Render.Keyboard (renderKeyboard)
import Musica.Render.Staff (renderStaff)
import Musica.Midi.NoteStream
import Musica.Midi.KeyMap
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad

-- stack runghc Main.hs -- --windowWidth 1920 --windowHeight 500 --deviceName "Midi Through Port-0" --middleCindex 60 --keysNumber 61

defaultWindowWidth    = fromMaybe 1920
defaultWindowHeight   = fromMaybe 600
defaultMiddleCindex   = fromMaybe 60
defaultKeysNumber     = fromMaybe 61
defaultStaffTopLines  = fromMaybe 3
defaultStaffBotLines  = fromMaybe 5
defaultStaffZeroTick  = fromMaybe 13
defaultStaffClefSize  = fromMaybe 5
defaultPanDivs        = PanDivs 0.45


data Config = Config { windowWidth    :: Maybe Int
                     , windowHeight   :: Maybe Int
                     , deviceName     :: String
                     , middleCindex   :: Maybe Int
                     , keysNumber     :: Maybe Int
                     , panDivs        :: [Float]
                     , staffTopLines  :: Maybe Int
                     , staffBotLines  :: Maybe Int
                     , staffZeroTicks :: Maybe Int
                     , staffClefSize  :: Maybe Int
                     , midiFile       :: FilePath
                     } deriving (Generic, Show)

instance ParseRecord Config

data PanDivs = PanDivs Float deriving Show

data Musica = Musica { viewPort       :: (Float, Float)
                     , heightDivs     :: PanDivs
                     , staffRenderer  :: Picture

                     , mousePos       :: (Float, Float)

                     , textureAdjust  :: ((Float, Float), (Float, Float), Float)
                     , textureFile    :: Maybe FilePath
                     , texturePic     :: Maybe Picture
                     } deriving Show

adjustTexture :: Monad m =>(Float, Float) ->Float ->Picture ->ExceptT String m Picture
adjustTexture (cx, cy) s = \case
  bm@(Bitmap w h _ _) ->return $ scale s s $ translate cx cy bm
  p                   ->throwE $ "Cannot adjust clef. Image picture expected but `" <> show p <> "`."

textureLoad :: (Float, Float) ->Float ->FilePath ->ExceptT String IO Picture
textureLoad center scale file = liftIO (loadJuicyPNG file) >>= \case
  Just p ->adjustTexture center scale p
  Nothing   ->throwE $ "Cannot load `" <> file <> "` image"

getMidiDevices :: IO [(DeviceID, DeviceInfo)]
getMidiDevices = do
  n <-countDevices
  mapM (\i ->(i,) <$> getDeviceInfo i) [0 .. n-1]

getMidiStream :: Config ->ExceptT String IO PMStream
getMidiStream cfg = do
  let devName = deviceName cfg
  selected <-filter (\(_,d) ->name d == devName && input d) <$> liftIO getMidiDevices
  case selected of
    [(i, _)] ->liftIO (openInput i) >>= either return (throwE . show)
    []       ->throwE $ "Input device `" <> devName <> "` not found"
    xs       ->throwE $ "Input devide `" <> devName <> "` exists " <> show (length xs) <> " times"

setupKeyMap :: Config ->PMStream ->ExceptT String IO KeyMap
setupKeyMap cfg stream = do
  km <-liftIO $ makeKeyMap (defaultMiddleCindex (middleCindex cfg)) (defaultKeysNumber (keysNumber cfg))
  liftIO $ updateKeyMapForever' 100 stream km
  return km

getPanDivs :: Monad m =>Config ->ExceptT String m PanDivs
getPanDivs cfg = case panDivs cfg of
                   []       ->return defaultPanDivs
                   xs@([a]) ->do
                                when (minimum xs <= 0) $ throwE "Pan division cannot be equal or less than zero"
                                when (maximum xs >= 1) $ throwE "Pan division cannot be equal or greater than one"
                                when (any (<= 0) (zipWith (-) (tail xs) xs)) $ throwE "Pan divisions must be in ascending order and not repeated"
                                return $ PanDivs a
                   _        ->throwE "Excepted one and only one height pannel division factor"

main' :: Config ->ExceptT String IO ()
main' cfg = do
  let hPath = "/srv/despierto/home/josejuan/Projects/Haskell/HMUSICA/hmusica/"
  stream              <-getMidiStream cfg
  km                  <-setupKeyMap cfg stream
  sol                 <-textureLoad (-34.0,  81.0) 1.45e-3 $ hPath <> "gfx/sol.png"
  fa                  <-textureLoad (-12.0, -50.0) 1.74e-3 $ hPath <> "gfx/fa.png"
  breve               <-textureLoad (  0.0,   0.0) 1.28e-3 $ hPath <> "gfx/breve.png"
  crotchet            <-textureLoad ( -1.0, 130.0) 1.48e-3 $ hPath <> "gfx/crotchet.png"
  demisemiquaver      <-textureLoad ( 47.0, 233.0) 1.11e-3 $ hPath <> "gfx/demisemiquaver.png"
  doublelonga         <-textureLoad ( -1.0, -85.0) 1.37e-3 $ hPath <> "gfx/doublelonga.png"
  hemidemisemiquaver  <-textureLoad ( 27.0, 127.0) 2.50e-3 $ hPath <> "gfx/hemidemisemiquaver.png"
  longa               <-textureLoad (  0.0, -89.0) 1.37e-3 $ hPath <> "gfx/longa.png"
  minim               <-textureLoad (  1.0, 130.0) 1.37e-3 $ hPath <> "gfx/minim.png"
  quaver              <-textureLoad ( 55.0, 172.0) 1.28e-3 $ hPath <> "gfx/quaver.png"
  semibreve           <-textureLoad ( -2.0,  -4.0) 1.28e-3 $ hPath <> "gfx/semibreve.png"
  semiquaver          <-textureLoad ( 42.0, 184.0) 1.37e-3 $ hPath <> "gfx/semiquaver.png"
  dot                 <-textureLoad (135.0,   1.0) 1.37e-3 $ hPath <> "gfx/dot.png"
  sharp               <-textureLoad (  0.0,   0.0) 1.28e-3 $ hPath <> "gfx/sharp.png"
  nkstream            <-liftIO $ noteStreamFromFile (midiFile cfg) "Piano"
  hdivs               <-getPanDivs cfg
  let vp@(vpw, vph) = (defaultWindowWidth (windowWidth cfg), defaultWindowHeight (windowHeight cfg))
      staffrenderer = renderStaff sol
                                  fa
                                  doublelonga
                                  longa
                                  breve
                                  semibreve
                                  minim
                                  crotchet
                                  quaver
                                  semiquaver
                                  demisemiquaver
                                  hemidemisemiquaver
                                  dot
                                  sharp
                                  (defaultStaffTopLines (staffTopLines cfg))
                                  (defaultStaffBotLines (staffBotLines cfg))
                                  (defaultStaffZeroTick (staffZeroTicks cfg))
                                  (defaultStaffClefSize (staffClefSize cfg))
                                  (defaultMiddleCindex (middleCindex cfg))
                                  (notes nkstream) (tempo nkstream) (480)
      musica        = Musica (fromIntegral vpw, fromIntegral vph) hdivs staffrenderer
                             (0,0) ((0, 0), (1, 1), 1) (Just $ hPath <> "songs/dot.png") Nothing
      window        = InWindow "HMusica" vp (0, 0)
  liftIO $ playIO window white 30 musica (renderMusica cfg km) interactiveMusica (const return)

main :: IO ()
main = do
  cfg <-getRecord "HMusica"
  runExceptT (main' cfg) >>= \case
    Left e  ->putStrLn $ "ERROR: " <> e
    Right () ->putStrLn $ "done"

renderMusica :: Config ->KeyMap ->Musica ->IO Picture
renderMusica cfg km mus = (renderMusica' cfg mus . map snd) <$> readKeyMap km

renderMusica' :: Config ->Musica ->[Bool] ->Picture
renderMusica' cfg mus st =
  let PanDivs hDiv1  = heightDivs mus
      (vw, vh)       = viewPort mus
      (xA)           = (-vw/2.2)
      (yA, yB, yC)   = (a, a * (1 - hDiv1) + b * hDiv1, b)
                       where m = 0.9
                             s = m * vh
                             a = 0.5 * s
                             b = -0.5 * s
      s1             = yA - yB
      s2             = yB - yC
      staff          = translate xA yA $ scale s1 s1 $ staffRenderer mus
      keyboard       = translate xA yB $ scale s2 s2 $ renderKeyboard st
      txt            = let ((px, py), _, _) = textureAdjust mus
                           ss = 1 / fromIntegral (12 + defaultStaffTopLines (staffTopLines cfg) + defaultStaffBotLines (staffBotLines cfg))
                       in  translate px py $ Pictures [scale s1 s1 $ scale ss ss $ fromMaybe Blank (texturePic mus), color red $ circle 2]
  in  Pictures [staff, keyboard, txt]

interactiveMusica :: Event ->Musica ->IO Musica
interactiveMusica (EventMotion mousePos') mus = return $ mus { mousePos = mousePos' }
interactiveMusica (EventKey (SpecialKey KeyF2) Up _ m) mus = let (_, c, s) = textureAdjust mus in interactiveReloadTexture $ mus { textureAdjust = (m, c, s) }
interactiveMusica (EventKey (SpecialKey KeyF3) Up _ m) mus = let (p, _, s) = textureAdjust mus in interactiveReloadTexture $ mus { textureAdjust = (p, m, s) }
interactiveMusica (EventKey (SpecialKey KeyF4) Up _ m) mus = let (p, c, _) = textureAdjust mus in interactiveReloadTexture $ mus { textureAdjust = (p, c, fst m) }
interactiveMusica  _                                   mus = return mus

interactiveReloadTexture :: Musica ->IO Musica
interactiveReloadTexture m = do
    let ((px, py), (cx, cy), dx) = textureAdjust m
        mf        = textureFile   m
    case mf of
      Nothing ->return m
      Just f ->do
              let c = (cx - px, cy - py)
                  s = (1 / (1 + dx - px))^2
              putStrLn $ "Adjust texture: " <> show c <> " " <> show s
              pic' <-runExceptT $ textureLoad c s f
              case pic' of
                Left e ->liftIO (putStrLn e) >> return m
                Right p ->return $ m { texturePic = Just $ scale 7 7 $ p }
