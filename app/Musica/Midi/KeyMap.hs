{-# LANGUAGE TupleSections #-}
module Musica.Midi.KeyMap (
  KeyMap
, makeKeyMap            -- new key map from MIDI input stream
, updateKeyMapForever'  -- update input events forever
, readKeyMap            -- read keys status
) where

import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Control.Concurrent
import Control.Monad
import Sound.PortMidi
import Foreign.C.Types (CLong)
import qualified Data.IntMap.Strict as Map

type KeyMap = MVar (Map.IntMap Bool)

makeKeyMap :: Int ->Int ->IO KeyMap
makeKeyMap middleCindex nkeys = newMVar $ Map.fromList $ (, False) <$> [minKeyCode .. maxKeyCode]
  where minKeyCode = 60 - middleCindex
        maxKeyCode = minKeyCode + nkeys - 1

collectKeyEvents :: PMStream ->IO [(Int, Bool)]
collectKeyEvents pms = do
  let keyEvent (PMMsg 128 key _       ) = Just (fromIntegral key, False)
      keyEvent (PMMsg 144 key pressure) = Just (fromIntegral key, pressure > 0)
      keyEvent  _                       = Nothing
  re <-readEvents pms
  case re of
    Left es ->return $ mapMaybe keyEvent $ (decodeMsg . message) <$> es
    Right _  ->return []

readKeyMap :: KeyMap ->IO [(Int, Bool)]
readKeyMap km = Map.assocs <$> readMVar km

updateKeyMap :: PMStream ->KeyMap ->IO ()
updateKeyMap pms km = do
  es <-collectKeyEvents pms
  modifyMVar_ km $ \m ->return (foldl' (\m (k, v) ->Map.adjust (const v) k m) m es)

updateKeyMapForever :: Int ->PMStream ->KeyMap ->IO ()
updateKeyMapForever timeInterval pms km = forever $ do
  threadDelay timeInterval
  updateKeyMap pms km

updateKeyMapForever' :: Int ->PMStream ->KeyMap ->IO ()
updateKeyMapForever' timeInterval pms km = void $ forkIO $ updateKeyMapForever timeInterval pms km

