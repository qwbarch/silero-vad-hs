#ifndef SILERO_DETECTOR_H
#define SILERO_DETECTOR_H

#include <stdio.h>
#include "model.h"
#include "math.h"
#include "../vec.h"

struct SpeechSegment {
  int start_index;
  int end_index;
  float start_time;
  float end_time;
};

struct VoiceDetector {
  struct SileroModel *model;
  float start_threshold;
  float end_threshold;
  float min_speech_samples;
  float max_speech_samples;
  float speech_pad_samples;
  float min_silence_samples;
  float min_silence_samples_at_max_speech;
};

void detect_segments(
  struct VoiceDetector *vad,
  const size_t samples_length,
  const float *samples,
  size_t *out_segments_length,
  struct SpeechSegment **out_segments
);

#endif
