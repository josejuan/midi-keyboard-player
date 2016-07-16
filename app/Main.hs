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

-- stack build hmusica --copy-bins && (cd hmusica && hmusica-exe --deviceName "Midi Through Port-0" --panDivs 0.69 --midiFile songs/silent_night.mid; cd ..)

defaultWindowWidth    = fromMaybe 1920
defaultWindowHeight   = fromMaybe 680
defaultMiddleCindex   = fromMaybe 60
defaultKeysNumber     = fromMaybe 61
defaultStaffTopLines  = fromMaybe 3
defaultStaffBotLines  = fromMaybe 5
defaultStaffZeroTick  = fromMaybe (2 * 1000)
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
                     , kbRenderer     :: [Bool] ->Picture
                     }

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
  stream     <-getMidiStream cfg
  km         <-setupKeyMap cfg stream
  sol        <-textureLoad (-34.0,  81.0) 1.45e-3 "gfx/sol.png"
  fa         <-textureLoad (-12.0, -50.0) 1.74e-3 "gfx/fa.png"
  dlonga     <-textureLoad ( -1.0, -85.0) 1.37e-3 "gfx/doublelonga.png"
  longa      <-textureLoad (  0.0, -89.0) 1.37e-3 "gfx/longa.png"
  breve      <-textureLoad (  0.0,   0.0) 1.28e-3 "gfx/breve.png"
  semibreve  <-textureLoad ( -2.0,  -4.0) 1.28e-3 "gfx/semibreve.png"
  minim      <-textureLoad (  1.0, 130.0) 1.37e-3 "gfx/minim.png"
  crotchet   <-textureLoad ( -1.0, 130.0) 1.48e-3 "gfx/crotchet.png"
  quaver     <-textureLoad ( 55.0, 172.0) 1.28e-3 "gfx/quaver.png"
  semiquaver <-textureLoad ( 42.0, 184.0) 1.37e-3 "gfx/semiquaver.png"
  dsquaver   <-textureLoad ( 47.0, 233.0) 1.11e-3 "gfx/demisemiquaver.png"
  hdsquaver  <-textureLoad ( 27.0, 127.0) 2.50e-3 "gfx/hemidemisemiquaver.png"
  dot        <-textureLoad (135.0,   1.0) 1.37e-3 "gfx/dot.png"
  sharp      <-textureLoad (  0.0,   0.0) 1.28e-3 "gfx/sharp.png"
  kbbg       <-textureLoad (  0.0,   0.0) (1/250) "gfx/keyboard.png"
  nkstream   <-liftIO $ noteStreamFromFile (midiFile cfg) "Piano"
  hdivs      <-getPanDivs cfg
  let vp@(vpw, vph) = (defaultWindowWidth (windowWidth cfg), defaultWindowHeight (windowHeight cfg))
      staffrenderer = renderStaff sol
                                  fa
                                  dlonga
                                  longa
                                  breve
                                  semibreve
                                  minim
                                  crotchet
                                  quaver
                                  semiquaver
                                  dsquaver
                                  hdsquaver
                                  dot
                                  sharp
                                  (defaultStaffTopLines (staffTopLines cfg))
                                  (defaultStaffBotLines (staffBotLines cfg))
                                  (defaultStaffZeroTick (staffZeroTicks cfg))
                                  (defaultStaffClefSize (staffClefSize cfg))
                                  (defaultMiddleCindex (middleCindex cfg))
                                  (tempo nkstream) (480) (notes nkstream)
      kbrenderer    = renderKeyboard kbbg
      musica        = Musica (fromIntegral vpw, fromIntegral vph) hdivs staffrenderer kbrenderer
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
      (yA, yB, yC)   = (a, a * (1 - hDiv1) + b * hDiv1, b) where { m = 0.9; s = m * vh; a = 0.5 * s; b = -0.5 * s }
      (s1, s2)       = (yA - yB, yB - yC)
      staff          = translate xA yA $ scale s1 s1 $ staffRenderer mus
      keyboard       = translate  0 yB $ scale s2 s2 $ kbRenderer mus st
  in  Pictures [staff, keyboard]

interactiveMusica :: Event ->Musica ->IO Musica
-- interactiveMusica (EventKey (SpecialKey KeyF2) Up _ m) mus =
interactiveMusica  _                                   mus = return mus
