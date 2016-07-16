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

test = do

  t <-Load.fromFile "hmusica/songs/Dvorak-Symphony9-2-from-the-New-World-piano.mid"
  let keyStreams = getKeyStream "Piano" t
  showT t
  return keyStreams

getKeyStream :: String ->T ->(Int, [(Int, Int, Int)])
getKeyStream name (Cons _ d ts) = (t, get $ head $ filter byName $ map toStreamList ts)
  where t = case d of
              Ticks tempo ->fromIntegral tempo
              SMPTE a b   ->a
        get ts = let es = reverse $ takeWhile notReset $ reverse ts
                 in  mapMaybe getNote es
        notReset (_, Event.MIDIEvent (Channel.Cons _ (Channel.Mode Mode.ResetAllControllers))) = False
        notReset _ = True
        byName = any f where f (_, Event.MetaEvent (Meta.TrackName name')) = name' == name
                             f _ = False
        getNote (t, Event.MIDIEvent (Channel.Cons _ (Channel.Voice (Voice.NoteOff p v)))) = Just (fromIntegral t, Channel.fromPitch p, Channel.fromVelocity v)
        getNote (t, Event.MIDIEvent (Channel.Cons _ (Channel.Voice (Voice.NoteOn  p v)))) = Just (fromIntegral t, Channel.fromPitch p, Channel.fromVelocity v)
        getNote _ = Nothing
        toStreamList ts = case E.viewL ts of
                            Just (e, ts) ->e: toStreamList ts
                            Nothing         ->[]


showT :: T ->IO ()
showT (Cons t d ts) = do
    putStrLn $ "Type: " ++ show t ++ " / Division: " ++ show d ++ " / Tracks " ++ show (length ts)
    mapM_ showTrack ts

showTrack t = do
    putStrLn "== TRACK =================================="
    showStream t

showStream t = case E.viewL t of
                 Nothing ->putStrLn "(end)" >> return undefined
                 Just ((t, b), ts) ->do
                                    case showEvent b of
                                      Nothing ->return ()
                                      Just e ->putStrLn $ "#" ++ show t ++ ": " ++ e
                                    showStream ts

showEvent (Event.MIDIEvent (Channel.Cons channel msgBody)) = (\b ->"MIDIEvent => " ++ show (Channel.fromChannel channel) ++ ", " ++ b) <$> showBody msgBody
showEvent (Event.MetaEvent meta) = Just $ showMetaEvent meta
showEvent (Event.SystemExclusive sysex) = error $ "showEvent (Event.SystemExclusive `" ++ show sysex ++ "`)"

showBody (Channel.Voice v) = showVoice v
showBody (Channel.Mode m) = Just $ show m

showVoice (Voice.NoteOff pitch velocity) = Nothing -- showNote "Off" pitch velocity
showVoice (Voice.NoteOn  pitch velocity) = Nothing -- showNote "On"  pitch velocity
-- showVoice (Voice.PolyAftertouch Channel.Pitch Voice.Pressure
showVoice (Voice.ProgramChange program) = Just $ "Program(" ++ show (Channel.fromProgram program) ++ ")"
showVoice (Voice.Control controller controllerValue) = Just $ "Control(" ++ show (Channel.fromController controller) ++ ", " ++ show controllerValue ++ ")"
-- showVoice (Voice.PitchBend Voice.PitchBendRange
-- showVoice (Voice.MonoAftertouch Voice.Pressure
showVoice x = error $ "showVoice not implemented `" ++ show x ++ "`"

showNote st pitch velocity = st ++ "/" ++ show (Channel.fromPitch pitch) ++ "/" ++ show (Channel.fromVelocity velocity)

showMetaEvent (Meta.TimeSig a b c d) = "TimeSig(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ", " ++ show d ++ ")"
showMetaEvent (Meta.KeySig (KeySignature.Cons KeySignature.Major (KeySignature.Accidentals acc))) = "M#" ++ show acc
showMetaEvent (Meta.KeySig (KeySignature.Cons KeySignature.Minor (KeySignature.Accidentals acc))) = "m#" ++ show acc
showMetaEvent (Meta.SetTempo t) = "Tempo(" ++ show t ++ ")"
showMetaEvent (Meta.TrackName n) = "TrackName(" ++ n ++ ")"
showMetaEvent x = error $ "showMetaEvent not implemented `" ++ show x ++ "`"
      -- Meta.SequenceNum Int
      -- Meta.TextEvent String
      -- Meta.Copyright String
      -- Meta.TrackName String
      -- Meta.InstrumentName String
      -- Meta.Lyric String
      -- Meta.Marker String
      -- Meta.CuePoint String
      -- Meta.MIDIPrefix Channel.Channel
      -- Meta.EndOfTrack
      -- Meta.SetTempo Meta.Tempo
      -- Meta.SMPTEOffset Meta.SMPTEHours Meta.SMPTEMinutes Meta.SMPTESeconds Meta.SMPTEFrames Meta.SMPTEBits
      -- Meta.TimeSig Int Int Int Int
      -- Meta.KeySig Sound.MIDI.KeySignature.T
      -- Meta.SequencerSpecific Sound.MIDI.IO.ByteList
      -- Meta.Unknown Int Sound.MIDI.IO.ByteList
