module Musica.Midi.NoteKind (
  NoteName(..)
, NoteType(..)
, NoteKind(..)
, NoteDurationMap
, makeNoteMap
, nearTo
) where

import qualified Data.Map.Strict as Map

type NoteDurationMap = Map.Map Int NoteKind

data NoteName = DoubleLonga
              | Longa
              | Breve
              | Semibreve
              | Minim
              | Crotchet
              | Quaver
              | Semiquaver
              | Demisemiquaver
              | Hemidemisemiquaver
                deriving (Eq, Show)

data NoteType = Simple | Dotted | DoubleDotted | TripleDotted deriving (Eq, Show)

data NoteKind = NoteKind { noteName   :: !NoteName
                         , noteType   :: !NoteType
                         } deriving (Eq, Show)

makeNoteTable :: Int ->[(Int, NoteKind)]
makeNoteTable crotchetDuration =
  let baseTable = [ (DoubleLonga        , (* 32))
                  , (Longa              , (* 16))
                  , (Breve              , (*  8))
                  , (Semibreve          , (*  4))
                  , (Minim              , (*  2))
                  , (Crotchet           , (*  1))
                  , (Quaver             , (`div`  2))
                  , (Semiquaver         , (`div`  4))
                  , (Demisemiquaver     , (`div`  8))
                  , (Hemidemisemiquaver , (`div` 16))
                  ]
      note (n, f) = let d = f crotchetDuration
                    in  [ (d                         , NoteKind n Simple      )
                        , (d + d `div` 2                 , NoteKind n Dotted      )
                        , (d + d `div` 2 + d `div` 4         , NoteKind n DoubleDotted)
                        , (d + d `div` 2 + d `div` 4 + d `div` 8 , NoteKind n TripleDotted)
                        ]
  in  concatMap note baseTable

makeNoteMap :: Int ->NoteDurationMap
makeNoteMap = Map.fromList . makeNoteTable

nearTo :: NoteDurationMap ->Int ->NoteKind
nearTo m d = case (Map.lookupLE d m, Map.lookupGE d m) of
               (Just (a, u), Just (b, v)) ->if abs (a - d) < abs (b - d) then u else v
               (Just (_, u), Nothing       ) ->u
               (Nothing       , Just (_, v)) ->v
               (Nothing       , Nothing       ) ->error "How are you constructed an empty NoteDurationMap?"

