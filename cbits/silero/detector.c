// Credits: SileroVAD's C# example:
// https://github.com/snakers4/silero-vad/blob/cb25c0c0470cf20d7f33805218831b9620dbb6a7/examples/csharp/SileroVadDetector.cs

#include "detector.h"
#include <stdio.h>

/** Split the audio samples into segments of <bold>WINDOW_SIZE</bold> size. */
vec_void_t split_frames(const size_t samples_length, const float *samples) {
  vec_void_t frames;
  vec_init(&frames);
  for (int i = 0; i < samples_length; i += WINDOW_SIZE) {
    float *frame = calloc(WINDOW_SIZE, WINDOW_SIZE_BYTES);
    int copy_bytes = WINDOW_SIZE_BYTES;
    if (i + WINDOW_SIZE_BYTES > samples_length) {
      copy_bytes = WINDOW_SIZE_BYTES - samples_length + i;
    }
    memcpy(frame, &samples[i], WINDOW_SIZE_BYTES);
    vec_push(&frames, frame);
  }
  return frames;
}

void reset_segment(struct SpeechSegment *segment) {
  segment->start_index = -1;
  segment->end_index = -1;
  segment->start_time = -1.0f;
  segment->end_time = -1.0f;
}

int math_min(int a, int b) { return (a < b) ? a : b; }
int math_max(int a, int b) { return (a > b) ? a : b; }

int compare_start_index(const void *a, const void *b) {
  return ((struct SpeechSegment *)a)->start_index -
         ((struct SpeechSegment *)b)->start_index;
}

float calculate_time(int index) {
  return floorf((float)index / (float)WINDOW_SIZE * 1000.0f) / 1000.0f;
}

vec_void_t merge_segments(vec_void_t original) {
  vec_void_t result;
  vec_init(&result);
  if (original.length == 0)
    return result;
  struct SpeechSegment *first_segment = original.data[0];
  int left = first_segment->start_index;
  int right = first_segment->end_index;
  if (original.length > 1) {
    vec_sort(&original, compare_start_index);
    for (int i = 0; i < original.length; i++) {
      struct SpeechSegment *segment = original.data[i];
      if (segment->start_index > right) {
        struct SpeechSegment *updated_segment =
            malloc(sizeof(struct SpeechSegment));
        updated_segment->start_index = left;
        updated_segment->end_index = right;
        updated_segment->start_time = calculate_time(left);
        updated_segment->end_time = calculate_time(right);
        vec_push(&result, updated_segment);
      }
    }
  }
  return result;
}

void detect_segments(struct SileroModel *model, const float start_threshold,
                     const float end_threshold, const float min_speech_samples,
                     const float max_speech_samples,
                     const float speech_pad_samples,
                     const float min_silence_samples,
                     const float min_silence_samples_at_max_speech,
                     const size_t samples_length, const float *samples,
                     size_t *out_segments_length,
                     struct SpeechSegment *out_segments) {
  vec_void_t frames = split_frames(samples_length, samples);
  vec_void_t segments;
  vec_init(&segments);
  bool triggered = false;
  int temp_end = 0;
  int previous_end = 0;
  int next_start = 0;
  struct SpeechSegment current_segment;
  reset_segment(&current_segment);
  for (int i = 0; i < frames.length; i++) {
    float *frame = frames.data[i];
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
        vec_push(&segments, &current_segment);
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
        vec_push(&segments, &current_segment);
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
          vec_push(&segments, &current_segment);
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
  vec_deinit(&frames);

  if (current_segment.start_index >= 0 &&
      WINDOW_SIZE - current_segment.start_index > min_speech_samples) {
    current_segment.end_index = WINDOW_SIZE;
    vec_push(&segments, &current_segment);
  }

  for (int i = 0; i < segments.length; i++) {
    struct SpeechSegment current_segment =
        *(struct SpeechSegment *)(segments.data[i]);
    if (i == 0) {
      current_segment.start_index =
          math_max(0, current_segment.start_index - speech_pad_samples);
    }
    if (i != segments.length - 1) {
      struct SpeechSegment *next_segment = segments.data[i + 1];
      int silence_duration =
          next_segment->start_index - current_segment.end_index;
      if (silence_duration < speech_pad_samples * 2) {
        current_segment.end_index += silence_duration / 2;
        next_segment->start_index =
            math_max(0, next_segment->start_index - silence_duration / 2);
      } else {
        current_segment.end_index = math_min(
            WINDOW_SIZE, current_segment.end_index + speech_pad_samples);
        next_segment->start_index =
            math_max(0, next_segment->start_index - speech_pad_samples);
      }
    } else {
      current_segment.end_index =
          math_min(WINDOW_SIZE, current_segment.end_index + speech_pad_samples);
    }
  }

  vec_void_t vec_result = merge_segments(segments);
  size_t result_bytes = vec_result.length * sizeof(struct SpeechSegment);
  out_segments = malloc(result_bytes);
  memcpy(out_segments, vec_result.data, result_bytes);
  *out_segments_length = vec_result.length;

  for (int i = 0; i < vec_result.length; i++) {
    free(vec_result.data[i]);
  }

  vec_deinit(&segments);
  vec_deinit(&vec_result);
}
