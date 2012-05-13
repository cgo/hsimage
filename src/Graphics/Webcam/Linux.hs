{-| The Linux interface for accessing webcams via video4linux.
This module uses the lower-level bindings to v4l to provide for a simple
way to access a web camera from Haskell code using repa. 
The module is meant to be used like shown in the file /example.hs/
in the root of the package.
For example:

> main = runCam (grab >>= saveBmp "1.bmp" >> 
>                grab >>= saveBmp "2.bmp" >> 
>                grab >>= saveBmp "3.bmp") (Webcam 0)

This example would take three pictures from the first webcam and
store them in three files.
-}

module Graphics.Webcam.Linux 
       ( 
         Webcam (..)
       , V4lCamT
       , CamState
       , liftIO
       , runCam
       , runCamWith
       , grab
       , grabF
       , rgbaToAbgr
       , flipY
       , getSize
       , getState
       , saveBmp
       , getImageFormat
       , findImageFormat
       ) where

import Graphics.Webcam.Linux.Internal
