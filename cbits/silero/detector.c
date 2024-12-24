// Credits: SileroVAD's C# example:
// https://github.com/snakers4/silero-vad/blob/cb25c0c0470cf20d7f33805218831b9620dbb6a7/examples/csharp/SileroVadDetector.cs

#include "detector.h"

int math_min(int a, int b) { return (a < b) ? a : b; }
int math_max(int a, int b) { return (a > b) ? a : b; }

/** Split the audio samples into segments of <bold>WINDOW_SIZE</bold> size. */
float **split_frames(const size_t samples_length, const float *samples) {
  printf("split start\n");
  fflush(stdout);
  float **frames = (float **)vector_create;
  printf("split for\n");
  fflush(stdout);
  for (int i = 0; i < samples_length; i += WINDOW_SIZE) {
    float *frame = calloc(WINDOW_SIZE, WINDOW_SIZE_BYTES);
    int copy_bytes = WINDOW_SIZE_BYTES;
    if (i + WINDOW_SIZE_BYTES > samples_length) {
      copy_bytes = WINDOW_SIZE_BYTES - samples_length + i;
    }
    memcpy(frame, &samples[i], WINDOW_SIZE_BYTES);
    vector_add(&frames, frame);
  }
  printf("split end\n");
  return frames;
}

void reset_segment(struct SpeechSegment *segment) {
  segment->start_index = -1;
  segment->end_index = -1;
  segment->start_time = -1.0f;
  segment->end_time = -1.0f;
}

int compare_start_index(const void *a, const void *b) {
  return ((struct SpeechSegment *)a)->start_index -
         ((struct SpeechSegment *)b)->start_index;
}

float calculate_time(int index) {
  return floorf((float)index / (float)WINDOW_SIZE * 1000.0f) / 1000.0f;
}

struct SpeechSegment *merge_segments(struct SpeechSegment *original) {
  struct SpeechSegment *result = vector_create();
  if (vector_size(original) == 0)
    return result;
  struct SpeechSegment first_segment = original[0];
  int left = first_segment.start_index;
  int right = first_segment.end_index;
  if (vector_size(original) > 1) {
    // vec_sort(&original, compare_start_index);
    for (int i = 0; i < vector_size(original); i++) {
      struct SpeechSegment segment = original[i];
      if (segment.start_index > right) {
        struct SpeechSegment updated_segment;
        updated_segment.start_index = left;
        updated_segment.end_index = right;
        updated_segment.start_time = calculate_time(left);
        updated_segment.end_time = calculate_time(right);
        vector_add(&result, updated_segment);
      }
    }
  }
  return result;
}

struct SpeechSegment *
detect_segments(struct SileroModel *model, const float start_threshold,
                const float end_threshold, const float min_speech_samples,
                const float max_speech_samples, const float speech_pad_samples,
                const float min_silence_samples,
                const float min_silence_samples_at_max_speech,
                const size_t samples_length, const float *samples,
                size_t *out_segments_length) {
  printf("before split frames\n");
  fflush(stdout);
  float **frames = split_frames(samples_length, samples);
  printf("after split frames\n");
  fflush(stdout);
  struct SpeechSegment *segments;
  bool triggered = false;
  int temp_end = 0;
  int previous_end = 0;
  int next_start = 0;
  struct SpeechSegment current_segment;
  reset_segment(&current_segment);
  for (int i = 0; i < vector_size(frames); i++) {
    float *frame = frames[i];
    float probability = detect_speech(model, WINDOW_SIZE, frame);

    if (probability >= start_threshold && temp_end != 0) {
      temp_end = 0;
      if (next_start < previous_end) {
        next_start = WINDOW_SIZE * i;
      }
    }

    if (probability >= start_threshold && !triggered) {
      triggered = true;
      current_segment.start_index = WINDOW_SIZE * i;
      continue;
    }

    if (triggered &&
        WINDOW_SIZE * i - current_segment.start_index > max_speech_samples) {
      if (previous_end != 0) {
        current_segment.end_index = previous_end;
        vector_add(&segments, current_segment);
        reset_segment(&current_segment);
        if (next_start < previous_end) {
          triggered = false;
        } else {
          current_segment.start_index = next_start;
        }
        previous_end = 0;
        next_start = 0;
        temp_end = 0;
      } else {
        current_segment.end_index = WINDOW_SIZE * i;
        vector_add(&segments, current_segment);
        reset_segment(&current_segment);
        previous_end = 0;
        next_start = 0;
        temp_end = 0;
        triggered = false;
      }
    }

    if (probability < end_threshold && triggered) {
      if (temp_end == 0) {
        temp_end = WINDOW_SIZE * i;
      }

      if (WINDOW_SIZE * i > temp_end > min_silence_samples_at_max_speech) {
        previous_end = temp_end;
      }

      if (WINDOW_SIZE * i - temp_end < min_silence_samples) {
        continue;
      } else {
        current_segment.end_index = temp_end;
        if (current_segment.end_index - current_segment.start_index >
            min_silence_samples) {
          vector_add(&segments, current_segment);
        }
        reset_segment(&current_segment);
        previous_end = 0;
        next_start = 0;
        temp_end = 0;
        triggered = false;
        continue;
      }
    }
    free(frame);
  }
  vector_free(frames);

  printf("minSpeechSamples: %f\n", min_speech_samples);
  fflush(stdout);

  printf("startThreshold: %f\n", start_threshold);
  fflush(stdout);

  printf("endThreshold: %f\n", end_threshold);
  fflush(stdout);

  printf("WINDOW_SIZE: %d\n", WINDOW_SIZE);
  fflush(stdout);

  printf("minSpeechSamples: %f\n", min_speech_samples);
  fflush(stdout);

  printf("maxSpeechSamples: %f\n", max_speech_samples);
  fflush(stdout);

  printf("speechPadSamples: %f\n", speech_pad_samples);
  fflush(stdout);

  printf("minSilenceSamples: %f\n", min_silence_samples);
  fflush(stdout);

  printf("minSilenceSamplesAtMaxSpeech: %f\n",
         min_silence_samples_at_max_speech);
  fflush(stdout);

  if (current_segment.start_index >= 0 &&
      WINDOW_SIZE - current_segment.start_index > min_speech_samples) {
    current_segment.end_index = WINDOW_SIZE;
    vector_add(&segments, current_segment);
  }

  for (int i = 0; i < vector_size(segments); i++) {
    struct SpeechSegment current_segment = segments[i];
    if (i == 0) {
      current_segment.start_index =
          math_max(0, current_segment.start_index - speech_pad_samples);
    }
    if (i != vector_size(segments) - 1) {
      struct SpeechSegment next_segment = segments[i + 1];
      int silence_duration =
          next_segment.start_index - current_segment.end_index;
      if (silence_duration < speech_pad_samples * 2) {
        current_segment.end_index += silence_duration / 2;
        next_segment.start_index =
            math_max(0, next_segment.start_index - silence_duration / 2);
      } else {
        current_segment.end_index = math_min(
            WINDOW_SIZE, current_segment.end_index + speech_pad_samples);
        next_segment.start_index =
            math_max(0, next_segment.start_index - speech_pad_samples);
      }
    } else {
      current_segment.end_index =
          math_min(WINDOW_SIZE, current_segment.end_index + speech_pad_samples);
    }
  }
  printf("segments: %lu\n", vector_size(segments));
  fflush(stdout);

  struct SpeechSegment *vec_result = merge_segments(segments);
  size_t result_bytes = vector_size(vec_result) * sizeof(struct SpeechSegment);
  struct SpeechSegment *out_segments = calloc(1, result_bytes);
  memcpy(out_segments, vec_result, result_bytes);
  *out_segments_length = vector_size(vec_result);

  printf("vec_result: %lu\n", vector_size(vec_result));
  fflush(stdout);

  for (int i = 0; i < vector_size(segments); i++) {
    segments[0].start_index = 1000;
    printf("vec_result[%d]=%d\n", i, segments[i].start_index);
    fflush(stdout);
  }

  vector_free(segments);
  vector_free(vec_result);
  return out_segments;
}
