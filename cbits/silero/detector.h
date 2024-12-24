#ifndef SILERO_DETECTOR_H
#define SILERO_DETECTOR_H

#include <stdbool.h>
#include <math.h>
#include "model.h"
#include "../vec.h"

struct SpeechSegment {
  int start_index;
  int end_index;
  float start_time;
  float end_time;
};

struct SpeechSegment *detect_segments(
  struct SileroModel *model,
  const float start_threshold,
  const float end_threshold,
  const float min_speech_samples,
  const float max_speech_samples,
  const float speech_pad_samples,
  const float min_silence_samples,
  const float min_silence_samples_at_max_speech,
  const size_t samples_length,
  const float *samples,
  size_t *out_segments_length
);

#endif
