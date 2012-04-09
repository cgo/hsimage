module Main where

import Graphics.Webcam.Linux
import Control.Monad (forM)
import Data.Array.Repa
import Control.Monad.IO.Class

-- main = runCam (forM [1..100] (const grab) >> grab >>= saveBmp "test.bmp") (Webcam 0)
main = runCam (grab >>= saveBmp "1.bmp" >> 
               grab >>= saveBmp "2.bmp" >> 
               grab >>= saveBmp "3.bmp") (Webcam 0) >>= putStrLn . show