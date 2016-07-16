module Musica.Midi.NoteStream (
  Stream(..)
, Note(..)
, NK.NoteName(..)
, NK.NoteType(..)
, NK.NoteKind(..)
, noteStreamFromFile
) where

import Sound.MIDI.File hiding (showEvent)
import Data.Maybe
import qualified Sound.MIDI.File.Load                   as Load
import qualified Sound.MIDI.File.Event.Meta             as Meta
import qualified Data.EventList.Relative.TimeBody       as E
import qualified Sound.MIDI.File.Event                  as Event
import qualified Sound.MIDI.Message.Channel             as Channel
import qualified Sound.MIDI.Message.Channel.Voice       as Voice
import qualified Sound.MIDI.Message.Channel.Mode        as Mode
import qualified Sound.MIDI.File.Event.SystemExclusive  as SystemExclusive
import qualified Sound.MIDI.KeySignature                as KeySignature
import qualified Sound.MIDI.Controller                  as Controller
import qualified Musica.Midi.NoteKind                   as NK

data Note = Note { timeIn   :: !Int
                 , duration :: !Int
                 , noteKind :: !NK.NoteKind
                 , note     :: !Int
                 , velocity :: !Int
                 } deriving Show

data Stream = Stream { tempo  :: !Int
                     , notes  :: [Note]
                     } deriving Show

noteStreamFromFile :: FilePath ->String ->IO Stream
noteStreamFromFile file trackName = toNoteStream <$> keyStreamFromFile file trackName

keyStreamFromFile :: FilePath ->String ->IO (Int, [(ElapsedTime, Event.T)], [(Int, Int, Int)])
keyStreamFromFile file trackName = getKeyStream trackName <$> Load.fromFile file

getKeyStream :: String ->T ->(Int, [(ElapsedTime, Event.T)], [(Int, Int, Int)])
getKeyStream name (Cons _ d ts) = (t, raw, notes)
  where t = case d of
              Ticks tempo ->fromIntegral tempo
              SMPTE a b   ->error "Computing tempo from `SMPTE` not implemented!"
        (raw, notes) = get $ head $ filter byName $ map toStreamList ts
        get ts = let es = reverse $ takeWhile notReset $ reverse ts
                 in  (es, mapMaybe getNote es)
        notReset (_, Event.MIDIEvent (Channel.Cons _ (Channel.Mode Mode.ResetAllControllers))) = False
        notReset _ = True
        byName = any f where f (_, Event.MetaEvent (Meta.TrackName name')) = name' == name
                             f _ = False
        getNote (t, Event.MIDIEvent (Channel.Cons _ (Channel.Voice (Voice.NoteOff p v)))) = Just (fromIntegral t, Channel.fromPitch p,                      0)
        getNote (t, Event.MIDIEvent (Channel.Cons _ (Channel.Voice (Voice.NoteOn  p v)))) = Just (fromIntegral t, Channel.fromPitch p, Channel.fromVelocity v)
        getNote _ = Nothing
        toStreamList ts = case E.viewL ts of
                            Just (e, ts) ->e: toStreamList ts
                            Nothing         ->[]

toNoteStream :: (Int, [(ElapsedTime, Event.T)], [(Int, Int, Int)]) ->Stream
toNoteStream (tempo, _, ks) = Stream tempo (acc 0 ks)
  where nkMap                 = NK.makeNoteMap tempo
        acc t ((dt, k, v):xs) = let t'           = t + dt
                                    xs'          = acc t' xs
                                    noteDuration = sum $ deltas xs
                                                   where deltas [] = []
                                                         deltas ((t, q, _): xs) = if q == k then [t] else t:deltas xs
                                    noteKind     = NK.nearTo nkMap noteDuration
                                in  if v > 0 && noteDuration > 0
                                      then Note t' noteDuration noteKind k v: xs'
                                      else                        xs'
        acc _             []  = []


