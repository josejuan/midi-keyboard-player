module Musica.Render.Staff (
  renderStaff
) where

import Graphics.Gloss
import Musica.Midi.NoteStream
import Control.Arrow

maxC = 100000
vlinecolor = makeColor 0.5 1 0.5 0.5

-- Output Picture will be 1 fixed height and width not bounded
renderStaff :: Picture
             ->Picture
             ->Picture
             ->Picture
             ->Picture
             ->Picture
             ->Picture
             ->Picture
             ->Picture
             ->Picture
             ->Picture
             ->Picture
             ->Picture
             ->Picture
             ->Int ->Int ->Int ->Int
             ->Int ->[Note] ->Int ->Int ->Picture
renderStaff clefSol
            clefFa
            noteDoublelonga
            noteLonga
            noteBreve
            noteSemibreve
            noteMinim
            noteCrotchet
            noteQuaver
            noteSemiquaver
            noteDemisemiquaver
            noteHemidemisemiquaver
            picDot
            sharp
            t b z w
            icrotchet ns tempo tpos = color black $ scale (1 / holes) (1 / holes) $ Pictures (vline: sol: fa: lines ++ notes ns)
  where t'      = fromIntegral t -- added top lines
        b'      = fromIntegral b -- added bottom lines
        z'      = fromIntegral z -- ticks to zero time vertical line
        w'      = fromIntegral w -- base note duration with in interline units
        tempo'  = 1 / fromIntegral tempo
        s       = 7
        holes   = 12 + t' + b'
        vline   = color vlinecolor $ translate (z'*w'-w'/2) (-(holes + 2)/2) $ rectangleSolid w' (holes + 2)
        lines   = [Line [(-maxC, -y), (maxC, -y)] | y <-[t'+1 .. t'+5] ++ [t'+7 .. t'+11]]
        sol     = translate 0 (-t'-4) $ scale s s $ clefSol
        fa      = translate 0 (-t'-8) $ scale s s $ clefFa
        rnote x n = translate x (0.5 * fromIntegral it - t' - 6) $ scale s s $ pictures [noteGfx $ noteKind n, translate (-0.15) 0 sh]
                    where (it, sh) = case (note n - icrotchet) `divMod` 12 of
                                       (d, 0 ) ->(d * 7 + 0, blank)
                                       (d, 1 ) ->(d * 7 + 0, sharp)
                                       (d, 2 ) ->(d * 7 + 1, blank)
                                       (d, 3 ) ->(d * 7 + 1, sharp)
                                       (d, 4 ) ->(d * 7 + 2, blank)
                                       (d, 5 ) ->(d * 7 + 3, blank)
                                       (d, 6 ) ->(d * 7 + 3, sharp)
                                       (d, 7 ) ->(d * 7 + 4, blank)
                                       (d, 8 ) ->(d * 7 + 4, sharp)
                                       (d, 9 ) ->(d * 7 + 5, blank)
                                       (d, 10) ->(d * 7 + 5, sharp)
                                       (d, 11) ->(d * 7 + 6, blank)
        notes (n:ns) = if t > tempo
                         then rnote x n: notes ns
                         else            notes ns
                       where t = timeIn n - tpos
                             x = w' * fromIntegral t * tempo'
        notes []     = []
        noteGfx (NoteKind DoubleLonga        dots) = noteDoublelonga
        noteGfx (NoteKind Longa              dots) = noteLonga
        noteGfx (NoteKind Breve              dots) = noteBreve
        noteGfx (NoteKind Semibreve          dots) = noteSemibreve
        noteGfx (NoteKind Minim              dots) = noteMinim
        noteGfx (NoteKind Crotchet           dots) = noteCrotchet
        noteGfx (NoteKind Quaver             dots) = noteQuaver
        noteGfx (NoteKind Semiquaver         dots) = noteSemiquaver
        noteGfx (NoteKind Demisemiquaver     dots) = noteDemisemiquaver
        noteGfx (NoteKind Hemidemisemiquaver dots) = noteHemidemisemiquaver
