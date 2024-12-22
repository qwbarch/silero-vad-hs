-- Credits: https://github.com/snakers4/silero-vad/blob/3780baf49f38b53c234ec90478171596c8a0c43f/examples/java-example/src/main/java/org/example/SlieroVadDetector.java
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Silero.Detector (
  DetectionInput (..),
  VoiceDetector,
  withDetector,
  resetDetector,
  detectSegments,
) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (unless, when)
import Control.Monad.Extra (unlessM, whenM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.State (MonadState (get), StateT, evalStateT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import GHC.Generics (Generic)
import Optics.Core ((%))
import Optics.State (use)
import Optics.State.Operators ((.=))
import Silero.Model (SileroModel, detectSpeech, resetModel, windowSize, withModel)

data SpeechSegment = SpeechSegment
  { startIndex :: Int
  , endIndex :: Int
  , startTime :: Float
  , endTime :: Float
  }
  deriving (Show, Eq, Ord, Generic)

data DetectionInput = DetectionInput
  { startThreshold :: Float
  -- ^ Threshold for speech start.
  , endThreshold :: Float
  -- ^ Threshold for speech end.
  , minSilenceSamples :: Int
  -- ^ Minimum number of silence samples to determine the end threshold of speech.
  , minSilenceSamplesAtMaxSpeech :: Int
  -- ^ TODO
  , speechPadSamples :: Int
  -- ^ Whether in the triggered state (i.e. whether is being detected).
  }
  deriving (Show, Eq, Ord, Generic)

data VoiceDetector = VoiceDetector
  { input :: DetectionInput
  , model :: SileroModel
  , currentSample :: Int
  , currentSegment :: SpeechSegment
  , segments :: [SpeechSegment]
  , tempEnd :: Int
  , triggered :: Bool
  , nextStart :: Int
  , previousEnd :: Int
  }
  deriving (Generic)

withDetector :: (MonadBaseControl IO m, MonadIO m) => DetectionInput -> StateT VoiceDetector m a -> m a
withDetector input f =
  withModel $ \model ->
    evalStateT f $
      VoiceDetector
        { input = input
        , model = model
        , currentSample = 0
        , currentSegment =
            SpeechSegment
              { startIndex = 0
              , endIndex = 0
              , startTime = 0.0
              , endTime = 0.0
              }
        , segments = []
        , tempEnd = 0
        , triggered = False
        , nextStart = 0
        , previousEnd = 0
        }

resetDetector :: (MonadState VoiceDetector m, MonadIO m) => m ()
resetDetector = do
  resetModel . model =<< get
  #currentSample .= 0
  #currentSegment
    .= SpeechSegment
      { startIndex = 0
      , endIndex = 0
      , startTime = 0.0
      , endTime = 0.0
      }
  #segments .= []
  #tempEnd .= 0
  #triggered .= False
  #nextStart .= 0
  #previousEnd .= 0

detectSegments :: (MonadState VoiceDetector m, MonadIO m) => Vector Float -> m ()
detectSegments samples = do
  let parseFrames index = do
        Vector.slice index windowSize samples
          : if index < Vector.length samples
            then parseFrames $ index + windowSize
            else mempty
  for_ (parseFrames 0) $ \frame -> do
    vad <- get
    probability <- detectSpeech vad.model frame

    if probability >= vad.input.startThreshold
      then do
        when (vad.tempEnd /= 0) $ do
          #tempEnd .= 0
          whenM (liftA2 (<) (use #nextStart) (use #previousEnd)) $
            #nextStart .= vad.currentSample * windowSize
        unless vad.triggered $ do
          #triggered .= True
          #currentSegment % #startIndex .= vad.currentSample * windowSize
          pure ()
      else
        if probability < vad.input.endThreshold && vad.triggered
          then do
            let elapsed = (vad.currentSample * windowSize -) <$> use #tempEnd
            whenM (use #tempEnd <&> (== 0)) $
              #tempEnd .= vad.currentSample * windowSize
            whenM (elapsed <&> (> vad.input.minSilenceSamplesAtMaxSpeech)) $ do
              (#previousEnd .=) =<< use #tempEnd
            unlessM (elapsed <&> (< vad.input.minSilenceSamples)) $ do
              pure ()
          else do
            pure ()

  resetDetector
