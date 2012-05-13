module Main where

import Graphics.Webcam.Linux
import Control.Monad (forM)
import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
-- import Control.Monad.IO.Class
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate


produceFrame :: CamState -> Float -> IO Picture
produceFrame s t = do
  e <- runCamWith s 
    (getSize >>= \(w,h) -> 
      grabF rgbaToAbgr >>= \fimg ->
      let bm = bitmapOfForeignPtr w h fp False 
          fp = toForeignPtr fimg
      in return bm)
  either (const $ error "ERROR!") (return) e
                    

-- main = runCam (forM [1..100] (const grab) >> grab >>= saveBmp "test.bmp") (Webcam 0)
main = runCam (grab >>= saveBmp "1.bmp" >> 
               grab >>= saveBmp "2.bmp" >> 
               grab >>= saveBmp "3.bmp" >>
               getSize >>= \(w,h) ->
                getState >>= \st ->
                liftIO (animateIO (InWindow "Hello, World!" (w,h) (10,10)) (makeColor 1 1 1 1) (produceFrame st))
              ) (Webcam 0) >>= putStrLn . show