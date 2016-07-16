module Musica.Render.Staff (
  renderStaff
) where

import Graphics.Gloss
import Musica.Midi.NoteStream
import Control.Arrow

maxC = 100000
vlinecolor = makeColor 0 1 0 0.75
maxNotes2Render = 100

-- Output Picture will be 1 fixed height and width not bounded
renderStaff :: Picture ->Picture ->Picture ->Picture ->Picture ->Picture ->Picture ->Picture ->Picture ->Picture ->Picture ->Picture ->Picture ->Picture
             ->Int ->Int ->Int ->Int
             ->Int ->Int ->Int ->[Note] ->Picture
renderStaff pSol pFa ntDL ntL ntB ntSB ntM ntC ntQ ntSQ ntDSQ ntHDSQ dot sharp topLns botLns zeroLn quaterW middleC tempo tpos ns =
  color black $ scale (1 / holes) (1 / holes) $ Pictures (vline: sol: fa: lines ++ notes ns)
  where t'      = fromIntegral topLns -- added top lines
        b'      = fromIntegral botLns -- added bottom lines
        w'      = fromIntegral quaterW -- base note duration width in interline units
        tempo'  = 1 / fromIntegral tempo
        holes   = 12 + t' + b'
        vline   = color vlinecolor $ Line [(x,0),(x,-h)] where { x = fromIntegral zeroLn * w' * tempo'; h = holes }
        lines   = [Line [(-maxC, -y), (maxC, -y)] | y <-[t'+1 .. t'+5] ++ [t'+7 .. t'+11]]
        sol     = translate 0 (-t'-4) $ scale 7 7 pSol
        fa      = translate 0 (-t'-8) $ scale 7 7 pFa
        rnote x n = translate x (0.5 * fromIntegral it - t' - 6) $ scale 7 7 $ pictures [noteGfx $ noteKind n, translate (-0.15) 0 sh]
                    where (it, sh) = case (note n - middleC) `divMod` 12 of
                                       (d,  0) ->(d * 7 + 0, blank)
                                       (d,  1) ->(d * 7 + 0, sharp)
                                       (d,  2) ->(d * 7 + 1, blank)
                                       (d,  3) ->(d * 7 + 1, sharp)
                                       (d,  4) ->(d * 7 + 2, blank)
                                       (d,  5) ->(d * 7 + 3, blank)
                                       (d,  6) ->(d * 7 + 3, sharp)
                                       (d,  7) ->(d * 7 + 4, blank)
                                       (d,  8) ->(d * 7 + 4, sharp)
                                       (d,  9) ->(d * 7 + 5, blank)
                                       (d, 10) ->(d * 7 + 5, sharp)
                                       (d, 11) ->(d * 7 + 6, blank)
        notes = map render . take maxNotes2Render . dropWhile (\n ->timeIn n < tempo + tpos)
                where render n = rnote x n where x = w' * fromIntegral (timeIn n - tpos) * tempo'
        noteGfx (NoteKind n dots) = case n of
                                      DoubleLonga        -> ntDL
                                      Longa              -> ntL
                                      Breve              -> ntB
                                      Semibreve          -> ntSB
                                      Minim              -> ntM
                                      Crotchet           -> ntC
                                      Quaver             -> ntQ
                                      Semiquaver         -> ntSQ
                                      Demisemiquaver     -> ntDSQ
                                      Hemidemisemiquaver -> ntHDSQ
