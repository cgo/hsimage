{-# LANGUAGE GeneralizedNewtypeDeriving, 
  TypeSynonymInstances, 
  FlexibleInstances,
  MultiParamTypeClasses, 
  ExistentialQuantification #-}

module Graphics.Webcam.Linux 
       ( 
         V4lCam'
       , Webcam (..)
       , runCam
       , grab
       , saveBmp
       , getImageFormat
       , findImageFormat
       ) where

import Graphics.Webcam.Linux.Internal

import Graphics.V4L2
import Data.Set (toList)
import Data.List (sortBy, unfoldr)
import Data.Maybe (fromJust)
import Data.Word
-- import Codec.Image.DevIL
import Control.Monad
-- import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Error
import Control.Exception
import Control.Applicative ((<*))

import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import Data.Array.Repa hiding ((++), toList)
import Data.Array.Repa.Eval
import qualified Data.Array.Repa.Eval as R (fromList)
import Data.Vector.Unboxed.Base
  -- import Data.Array.Repa.IO.DevIL



