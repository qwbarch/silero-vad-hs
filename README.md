# silero-vad-hs

[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT) [![Hackage](http://img.shields.io/hackage/v/silero-vad.svg)](https://hackage.haskell.org/package/silero-vad)

Voice activity detection powered by [SileroVAD](https://github.com/snakers4/silero-vad), full credits to [snakers4](https://github.com/snakers4).

## Supported architectures

Tested on ``GHC 9.8``, ``GHC 9.2.8``, and ``GHC 8.10.7``.

- [![build-linux-x64](https://github.com/qwbarch/silero-vad-hs/actions/workflows/linux-x64.yml/badge.svg)](https://github.com/qwbarch/silero-vad-hs/actions/workflows/linux-x64.yml)
- [![build-mac-arm64](https://github.com/qwbarch/silero-vad-hs/actions/workflows/mac-arm64.yml/badge.svg)](https://github.com/qwbarch/silero-vad-hs/actions/workflows/mac-arm64.yml)
- [![build-mac-x64](https://github.com/qwbarch/silero-vad-hs/actions/workflows/mac-x64.yml/badge.svg)](https://github.com/qwbarch/silero-vad-hs/actions/workflows/mac-x64.yml)
- [![build-windows-x64](https://github.com/qwbarch/silero-vad-hs/actions/workflows/windows-x64.yml/badge.svg)](https://github.com/qwbarch/silero-vad-hs/actions/workflows/windows-x64.yml)

## Quick start

This is a literate haskell file. You can run this example via the following:
```bash
nix develop --command bash -c '
  export LD_LIBRARY_PATH=lib:$(nix path-info .#stdenv.cc.cc.lib)/lib
  cabal run --flags="build-readme"
'
```

Necessary language extensions and imports for the example:
```haskell
import qualified Data.Vector.Storable as Vector
import Data.Function ((&))
import Data.WAVE (sampleToDouble, WAVE (waveSamples), getWAVEFile)
import Silero (withVad, withModel, detectSegments, detectSpeech, windowLength)
```

For this example, the [WAVE](https://hackage.haskell.org/package/WAVE) library is used for simplicity.  
Unfortunately, its design is flawed and represents audio in a lazy linked list.  
Prefer using [wave](https://hackage.haskell.org/package/wave) for better performance.

```haskell
main :: IO ()
main = do
  wav <- getWAVEFile "lib/jfk.wav"
```
The functions below expects a ``Vector Float``. This converts it to the expected format.
```haskell
  let samples =
        concat (waveSamples wav)
          & Vector.fromList
          & Vector.map (realToFrac . sampleToDouble)
```
Use ``detectSegments`` to detect the start/end times of voice activity segments.
```haskell
  withVad $ \vad -> do
    segments <- detectSegments vad samples
    print segments
```
Alternatively, use ``detectSpeech`` if you want to detect if speech is found in a single window.  
```haskell
  withModel $ \model -> do
    probability <- detectSpeech model $ Vector.take windowLength samples
    putStrLn $ "Probability: " <> show probability
```

> [!NOTE]
> Audio passed to ``detectSegments`` and ``detectSpeech`` functions have the following requirements:
> - Must be 16khz sample rate.
> - Must be mono channel.
> - Must be 16-bit audio.
>
> When using ``detectSpeech``, audio samples must be of size ``windowLength`` (defined as 512).  
> If ``length samples /= windowLength``, the probability will always be 0.
