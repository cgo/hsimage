{-# LANGUAGE GeneralizedNewtypeDeriving, 
  TypeSynonymInstances, 
  FlexibleInstances,
  FlexibleContexts,
  MultiParamTypeClasses, 
  ExistentialQuantification #-}

module Graphics.Webcam.Linux.Internal
       ( 
         Image
       , V4lCam' (..)
       , InnerMonad (..)
       , getState
       , setState
       , getDev
       , camToName
       , openCam
       , closeCam
       , Webcam (..)
       , runCam
       , grab
       , saveBmp
       , getImageFormat
       , setSize
       , getSize
       , chooseSize
       , frameToRGBA
       , findImageFormat
       , CamState (..)
       ) where

import Graphics.V4L2
import qualified Data.Set as S (toList)
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
import Control.DeepSeq

import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import Data.Array.Repa hiding ((++))
import Data.Array.Repa.Eval
import qualified Data.Array.Repa.Eval as R (fromList)
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.Repr.ByteString
import Data.Vector.Unboxed.Base
import Data.ByteString (pack)
  -- import Data.Array.Repa.IO.DevIL

import qualified Codec.BMP as BMP

type Image r a = Array r DIM3 a


data Webcam = Webcam Int

-- class Monad m => Webcam a m where
--   webcamGetSizes :: a -> m [(Int, Int)]
--   webcamSetSize :: a -> (Int, Int) -> m ()
--   webcamGrab :: a -> m (Image Word8)


data CamState = CamState { 
  camstateCam    :: Webcam,
  camstateDev    :: Maybe Device,
  camstateFormat :: Maybe ImageFormat
  }
                             

data V4lCam' m a = V4lCam' { unV4lCam :: InnerMonad m a }


type InnerMonad m a = ErrorT String (StateT CamState m) a


instance Monad m => Monad (V4lCam' m) where
  f >>= g = V4lCam' $ unV4lCam f >>= (unV4lCam . g)
  return = V4lCam' . return
  

instance Monad m => Functor (V4lCam' m) where
  fmap f act = act >>= return . f
    
               
instance Monad m =>  MonadPlus (V4lCam' m) where
  mzero = V4lCam' $ mzero
  mplus (V4lCam' a) (V4lCam' b) = V4lCam' $ mplus a b


instance MonadIO m => MonadIO (V4lCam' m) where
    liftIO = V4lCam' . lift . lift . liftIO


instance Monad m => MonadError String (V4lCam' m) where
    throwError = V4lCam' . throwError
    catchError (V4lCam' act) errf = V4lCam' $ catchError act (unV4lCam . errf)      


getState :: Monad m => V4lCam' m CamState
getState = V4lCam' $ lift get


setState :: Monad m => CamState -> V4lCam' m ()
setState = V4lCam' . lift . put


getDev :: Monad m => V4lCam' m Device
getDev = camstateDev `fmap` getState >>= maybe (throwError "Device not open.") return



saveBmp :: (MonadIO m, Repr r Word8) => FilePath -> Image r Word8 -> V4lCam' m ()
saveBmp name i = do
  -- This does not compile; why not? Fill instances missing in Repa?
  -- a <- liftIO $ ((copyP i :: IO (Image B Word8)) >>=
  --                return . toByteString >>= 
  --                return . BMP.packRGBA32ToBMP w h >>=
  --                BMP.writeBMP name >> return True) `onException` return False

  a <- liftIO $ (let bs = pack (toList i); bsa = BMP.packRGBA32ToBMP w h bs
                 in 
                  BMP.writeBMP name bsa >> return True) `onException` return False
  if not a 
    then throwError ("Could not save image " ++ name)
    else return ()
  where (Z :. h :. w :. k) = extent i
                 


camToName :: Webcam -> String
camToName (Webcam n) = "/dev/video" ++ show n


openCam :: MonadIO m => Webcam -> V4lCam' m ()
openCam w = do
  let name = camToName w
  mdev <- liftIO $ Just `fmap` openDevice name `onException` return Nothing
  case mdev of
    Just dev -> V4lCam' $ lift $ modify $ \s -> s { camstateDev = Just dev }
    _        -> throwError $ "Could not open device " ++ name


closeCam :: MonadIO m => V4lCam' m ()
closeCam = getDev >>= liftIO . closeDevice


runCam :: MonadIO m => V4lCam' m a -> Webcam -> m (Either String a)
runCam act cam = r
    where 
      uact = unV4lCam ((openCam cam >> findImageFormat >> act >>= \a -> closeCam >> return a) 
                       `catchError` 
                       (\e -> liftIO (putStrLn "final close!") >> closeCam >> throwError e))
      eact = runErrorT uact
      r    = evalStateT eact s
      s    = CamState cam Nothing Nothing


grab :: MonadIO m => V4lCam' m (Image U Word8)
grab = do 
  dev <- getDev
  format <- getImageFormat
  liftIO $ withFrame dev format (\p i -> frameToRGBA format p >>= computeP)
  -- runIL (writeImage "testimage.jpg" img)

    
getImageFormat :: Monad m => V4lCam' m ImageFormat
getImageFormat = camstateFormat `fmap` getState >>= maybe (throwError "Format is not set.") return


setSize :: (Int, Int) -> V4lCam' m ()
setSize (w,h) = undefined


getSize :: V4lCam' m (Int, Int)
getSize = undefined


--------------------------------------------------------------------------------


chooseSize :: FrameSizes -> Maybe FrameSize
chooseSize (DiscreteSizes s) = let l = reverse $ sortBy (\a b -> compare (frameWidth a) (frameWidth b)) $ S.toList s
                         in maybeMedian l
chooseSize (StepwiseSizes minW maxW stepW minH maxH stepH) = Just $ FrameSize maxW maxH


maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (a:_) = Just a

maybeMedian :: [a] -> Maybe a
maybeMedian [] = Nothing
maybeMedian l = Just $ l !! n 
  where n = m - 1 - m `div` 4
        m = length l
                                    

findImageFormat :: MonadIO m => V4lCam' m ()
findImageFormat = do
  dev <- getDev
  (info,cap) <- liftIO $ deviceInfoCapabilities dev
  liftIO $ putStrLn $ show cap
  liftIO $ setVideoInput dev (fromIntegral 0)
         
  sizes <- liftIO $ queryFrameSizes dev PixelRGB24
  format' <- liftIO $ getFormat dev Capture
  (FrameSize sx sy) <- let mf = chooseSize sizes
                       in case mf of
                         Just s -> return s 
                         _      -> throwError "No suitable size found!"
  let format = format' { imageWidth = sx,
                         imageHeight = sy,
                         imagePixelFormat = PixelRGB24 }
  liftIO $ putStrLn ("Size: " ++ show sx ++ " , " ++ show sy)
  liftIO $ setFormat dev Capture format                       
  format <- liftIO $ getFormat dev Capture
  liftIO $ putStrLn $ show format
  getState >>= \s -> setState (s { camstateFormat = Just format } )


flipY :: Repr r a => Image r a -> Image D a
flipY i = backpermute sh (\(Z :. y :. x :. j) -> Z :. h - 1 - y :. x :. j) i
  where sh@(Z :. h :. w :. k) = extent i


rgbToRgba :: Num a => Repr r a => Image r a -> Image D a
rgbToRgba src | k == 3 = flipY $ traverse src shf f
              | k == 4 = delay src
              | otherwise = error "Could not convert image to rgba"
  where shf _                                = (Z :. h :. w :. 4)
        (Z :. h :. w :. k)                   = extent src
        f g p@(Z :. _ :. _ :. j) | j == 3     = 255
                                 | otherwise = g p


{-| Copy the image stored at the given pointer in the given format into a 4-channel RGBA image.
This currently works only for 3-channel source images which are stored contiguously.
There may be no line padding. -}
frameToRGBA :: (Data.Vector.Unboxed.Base.Unbox a, Num a, Elt a, Storable a) => ImageFormat -> Ptr a -> IO (Image D a)
frameToRGBA i p = do
  fp <- newForeignPtr_ p
  let src = fromForeignPtr sh fp 
      sh  = Z :. h :. w :. 3
      w   = imageWidth i
      h   = imageHeight i
  return $ rgbToRgba src


-- Kleiner Uebungstext zwischendurch:
-- Was man nicht alles macht, um ein kleines bisschen besser zu werden.
-- Wenn ich mich sehr konzentriere, geht Colemak schon erklecklich schnell.
-- Ich muss aber immernoch auf meiner alten Tastatur das 'c' mit dem Zeigefinger anschlagen,
-- weil die Bewegung mit dem Mittelfinger unangenehm ist. Auf der TypeMatrix-Tastatur wird das wohl anders sein.
-- Das 'e' und das 'i' verwechsle ich noch manchmal, genauso wie das 's' und das 'r',
-- wenn ich mich nicht konzentriere.
-- Ich habe leider keine Ahnung, wie schnell ich mit qwerty bin, so dass ich keinen direkten Vergleich habe.
-- äääääääh... Moment mal...


{-
Noch ein kleiner Colemak-Übungstext zwischendurch.
Je später es wird, desto schwieriger wird das tippen.
Ich mache langsam immer mehr Fehler und versuche, immer schneller zu tippen.
Das vertauschte N macht mich noch wahnsinnig ;) kkkkkkk 
KTouch hat all seine Lektionen verschossen und wieder von vorne angefangen.
Immerhin konnte ich jetzt schon in den ersten 5 Lektionen einen Fortschritt feststellen.
97% Trefferquote und 63 Worte pro Minute (179 Anschläge). Ich vermute, dass das in den höheren 
Lektionen wieder etwas langsamer wird, wenn alle Tasten dazukommen und 
die Wörter länger sind, aber das ist schonmal ein Lichtblick. -}

{-| Copies an image from the camera frame to an RGBA 'Repa' array. -}
-- frameToRGBAOld :: (Data.Vector.Unboxed.Base.Unbox a, Num a, Elt a, Storable a) => ImageFormat -> Ptr a -> IO (Image a)
-- frameToRGBAOld i p = do
--   let ps = reverse $ take h $ iterate (\a -> plusPtr a inc) p
--       inc = imageBytesPerLine i
--       peekLine l p = do
--         let ps = take l $ iterate (flip advancePtr 1) p
--             f ([],k') | k' == 0 = Just (255, ([], 3))
--                       | otherwise = Nothing
--             f (xs@(x:r),k') | k' == 0 = Just (255, (xs, 3))
--                             | otherwise = let k'' = k' - 1 
--                                           in 
--                                            Just (x, (r, k''))
                        
--         els <- mapM peek ps
--         return $ unfoldr f (els,3)
        
--       (w,h) = (imageWidth i, imageHeight i)
--       sh = shapeOfList [k,w,h]
--       k = 4
--   els <- concat `fmap` mapM (peekLine (w*3)) ps
--   now $ R.fromList sh els
