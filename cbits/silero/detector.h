#ifndef SILERO_DETECTOR_H
#define SILERO_DETECTOR_H

#include <stdbool.h>
#include <math.h>
#include "model.h"
#include "../vec.h"

struct VoiceDetector {
  struct SileroModel *model;
  const float start_threshold;
  const float end_threshold;
  const int min_speech_samples;
  const int max_speech_samples;
  const int speech_pad_samples;
  const int min_silence_samples;
  const int min_silence_samples_at_max_speech;
};

struct SpeechSegment {
  int start_index;
  int end_index;
  int start_time;
  int end_time;
};

void detect_segments(struct VoiceDetector *vad, const size_t samples_length, const float *samples, size_t *out_segments_length, struct SpeechSegment *out_segments);

#endif
