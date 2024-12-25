// Credits: SileroVAD's C# example:
// https://github.com/snakers4/silero-vad/blob/cb25c0c0470cf20d7f33805218831b9620dbb6a7/examples/csharp/SileroVadDetector.cs

#include "detector.h"

void reset_segment(struct SpeechSegment *segment) {
  segment->start_index = -1;
  segment->end_index = -1;
  segment->start_time = -1.0f;
  segment->end_time = -1.0f;
}

int math_min(int a, int b) { return (a < b) ? a : b; }
int math_max(int a, int b) { return (a > b) ? a : b; }

// int compare_start_index(const void *a, const void *b) {
//   return ((struct SpeechSegment *)a)->start_index -
//          ((struct SpeechSegment *)b)->start_index;
// }

void detect_segments(struct SileroModel *model, const float start_threshold,
                     const float end_threshold, const float min_speech_samples,
                     const float max_speech_samples,
                     const float speech_pad_samples,
                     const float min_silence_samples,
                     const float min_silence_samples_at_max_speech,
                     const size_t samples_length, const float *samples,
                     size_t *out_segments_length,
                     struct SpeechSegment **out_segments) {

  size_t frames_length = samples_length / WINDOW_LENGTH;
  for (int i = 0; i < frames_length; i++) {
    const float *frame = samples + i * WINDOW_LENGTH;
    float probability = detect_speech(model, frame);
    printf("probability: %f\n", probability);
    fflush(stdout);
  }

  *out_segments_length = 1;
  // TODO: don't need calloc later when actually setting the values
  *out_segments = calloc(*out_segments_length,
                         *out_segments_length * sizeof(struct SpeechSegment));
  (*out_segments[0]).start_time = 1.2345;
  (*out_segments[0]).end_index = -2345;
}
