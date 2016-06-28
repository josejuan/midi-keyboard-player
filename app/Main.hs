{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Options.Generic
import Sound.PortMidi
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Musica.Render.Keyboard (renderKeyboard)
import Musica.Midi.KeyMap

data Config = Config { windowWidth  :: Int
                     , windowHeight :: Int
                     , deviceName   :: String
                     , middleCindex :: Int
                     , keysNumber   :: Int
                     } deriving (Generic, Show)

instance ParseRecord Config

data Musica = Musica { mousePos :: (Float, Float)
                     , viewPort :: (Float, Float)
                     } deriving Show

main :: IO ()
main = do
  cfg <-getRecord "HMusica"
  devices <-countDevices >>= mapM  getDeviceInfo . enumFromTo 0 . subtract 1
  case [id | (id, dev) <-zip [0..] devices, name dev == deviceName cfg, input dev] of
    [id] ->openInput id >>= \case
                Left stream ->mainLoop cfg stream
                Right e ->error $ "Cannot open device. Reason: " ++ show e
    _ ->error $ "Input device `" ++ deviceName cfg ++ "` not found!"

mainLoop :: Config ->PMStream ->IO ()
mainLoop cfg stream = do
  km <-makeKeyMap (middleCindex cfg) (keysNumber cfg)
  updateKeyMapForever' 100 stream km
  let vp@(vpw, vph) = (windowWidth cfg, windowHeight cfg)
      musica = Musica (0, 0) (fromIntegral vpw, fromIntegral vph)
  playIO (InWindow "HMusica" vp (0, 0)) white 30 musica (renderMusica km) interactiveMusica (const return)

renderMusica :: KeyMap ->Musica ->IO Picture
renderMusica km mus = (renderMusica' mus . map snd) <$> readKeyMap km

renderMusica' :: Musica ->[Bool] ->Picture
renderMusica' mus st = let (px, py) = mousePos mus
                           (vw, vh) = viewPort mus
                       in  translate (-vw/2.2) (vh/2.2) $ scale (0.9*vw) (0.9*vw) $ renderKeyboard st

interactiveMusica :: Event ->Musica ->IO Musica
interactiveMusica (EventMotion mousePos') mus = return $ mus { mousePos = mousePos' }
interactiveMusica  _                      mus = return   mus

