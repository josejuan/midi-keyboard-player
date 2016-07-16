module Musica.Render.Keyboard (
  renderKeyboard
) where

import Data.List (genericLength)
import Graphics.Gloss

whiteHeight = 14
whiteWidth  = 2.3
blackHeight = 9
blackWidth  = 1.5

bww = 2 * blackWidth  / whiteWidth
bwh =     blackHeight / whiteHeight

keyRatio = whiteHeight / whiteWidth

-- Output Picture will be 1 fixed height and width proportional to keys size.
renderKeyboard :: [Bool] ->Picture
renderKeyboard xs = scale (0.5 / keyRatio) (-1) $ translate 1 0 $ Pictures (blancas ++ negras)
  where blancas = renderKeyStream odd  white   2   1 xs
        negras  = renderKeyStream even black bww bwh xs
        nteclas = genericLength blancas

renderKeyStream :: (Int ->Bool) ->Color ->Float ->Float ->[Bool] ->[Picture]
renderKeyStream f c w h xs = [render k b | (k, b) <-zip keysPos xs, f k]
  where render k b = translate (fromIntegral k - 1) (h / 2)
                   $ pictures [ color (if b then green else c) $ rectangleSolid w h
                              , color black                    $ rectangleWire  w h ]
        keysPos = [i | i <-[1..], let z = i `mod` 14, z /= 0 && z /= 6]

