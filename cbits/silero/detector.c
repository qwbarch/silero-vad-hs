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

int compare_segment(const void *a, const void *b) {
  return ((struct SpeechSegment *)a)->start_index -
         ((struct SpeechSegment *)b)->start_index;
}

float calculate_time(int offset) {
  return floorf(offset / (float)SAMPLE_RATE * 1000.0f) / 1000.0f;
}

void merge_and_calculate_time(vector(struct SpeechSegment) * in_segments,
                              vector(struct SpeechSegment) * out_segments) {
  if (vector_size(*in_segments) == 0)
    return;
  int left = (*in_segments)[0].start_index;
  int right = (*in_segments)[0].end_index;
  if (vector_size(*in_segments) > 1) {
    qsort(*in_segments, vector_size(*in_segments), sizeof(struct SpeechSegment),
          compare_segment);
    for (int i = 0; i < vector_size(*in_segments); i++) {
      struct SpeechSegment *segment = &(*in_segments)[i];
      if (segment->start_index > right) {
        struct SpeechSegment out_segment;
        out_segment.start_index = left;
        out_segment.end_index = right;
        out_segment.start_time = calculate_time(left);
        out_segment.end_time = calculate_time(right);
        vector_add(out_segments, out_segment);
        fflush(stdout);
        left = segment->start_index;
        right = segment->end_index;
      } else {
        right = math_max(right, segment->end_index);
      }
    }
  }
  struct SpeechSegment out_segment;
  out_segment.start_index = left;
  out_segment.end_index = right;
  out_segment.start_time = calculate_time(left);
  out_segment.end_time = calculate_time(right);
  vector_add(out_segments, out_segment);
}

void detect_segments(struct SileroModel *model, const float start_threshold,
                     const float end_threshold, const float min_speech_samples,
                     const float max_speech_samples,
                     const float speech_pad_samples,
                     const float min_silence_samples,
                     const float min_silence_samples_at_max_speech,
                     const size_t samples_length, const float *samples,
                     size_t *out_segments_length,
                     struct SpeechSegment **out_segments) {

  size_t frames_length = ceil((double)samples_length / (double)WINDOW_LENGTH);
  struct SpeechSegment current_segment;
  reset_segment(&current_segment);
  vector(struct SpeechSegment) segments = vector_create();
  int temp_end = 0;
  int next_start = 0;
  int previous_end = 0;
  bool triggered = false;
  for (int i = 0; i < frames_length; i++) {
    const int offset = i * WINDOW_LENGTH;
    const bool is_last_frame = offset == (frames_length - 1) * WINDOW_LENGTH;
    float *frame = is_last_frame ? calloc(WINDOW_LENGTH, WINDOW_BYTES)
                                 : (float *)(samples + offset);
    if (is_last_frame) {
      memcpy(frame, samples + offset,
             samples_length - (frames_length - 1) * WINDOW_LENGTH);
    }
    float probability = detect_speech(model, frame);

    if (is_last_frame) {
      free(frame);
    }

    if (probability >= start_threshold && temp_end != 0) {
      temp_end = 0;
      if (next_start < previous_end) {
        next_start = i * WINDOW_LENGTH;
      }
    }

    if (probability >= start_threshold && !triggered) {
      triggered = true;
      current_segment.start_index = i * WINDOW_LENGTH;
      continue;
    }

    if (triggered &&
        i * WINDOW_LENGTH - current_segment.start_index > max_speech_samples) {
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
        current_segment.end_index = i * WINDOW_LENGTH;
        vector_add(&segments, current_segment);
        reset_segment(&current_segment);
        previous_end = 0;
        next_start = 0;
        temp_end = 0;
        triggered = false;
        continue;
      }
    }

    if (probability < end_threshold && triggered) {
      if (temp_end == 0) {
        temp_end = i * WINDOW_LENGTH;
      }
      if (i * WINDOW_LENGTH - temp_end > min_silence_samples_at_max_speech) {
        previous_end = temp_end;
      }
      if (i * WINDOW_LENGTH - temp_end < min_silence_samples) {
        continue;
      } else {
        current_segment.end_index = temp_end;
        if (current_segment.end_index - current_segment.start_index >
            min_silence_samples) {
          vector_add(&segments, current_segment);
          reset_segment(&current_segment);
          previous_end = 0;
          next_start = 0;
          temp_end = 0;
          triggered = false;
          continue;
        }
      }
    }
  }

  if (current_segment.start_index >= 0 &&
      samples_length - current_segment.start_index > min_speech_samples) {
    current_segment.end_index = samples_length;
    vector_add(&segments, current_segment);
  }

  for (int i = 0; i < vector_size(segments); i++) {
    struct SpeechSegment *segment = &segments[i];
    if (i == 0) {
      segment->start_index =
          math_max(0, segment->start_index - speech_pad_samples);
    }
    if (i != vector_size(segments) - 1) {
      struct SpeechSegment *next_segment = &segments[i + 1];
      int silence_duration = next_segment->start_index - segment->end_index;
      if (silence_duration < 2 * speech_pad_samples) {
        segment->end_index += silence_duration / 2;
        next_segment->start_index =
            math_max(0, next_segment->start_index - silence_duration / 2);
      } else {
        segment->end_index =
            math_min(samples_length, segment->end_index + speech_pad_samples);
        next_segment->start_index =
            math_max(0, next_segment->start_index - speech_pad_samples);
      }
    } else {
      segment->end_index =
          math_min(samples_length, segment->end_index + speech_pad_samples);
    }
  }

  vector(struct SpeechSegment) merged_segments = vector_create();
  merge_and_calculate_time(&segments, &merged_segments);

  *out_segments_length = vector_size(merged_segments);
  *out_segments = malloc(*out_segments_length * sizeof(struct SpeechSegment));
  memcpy(*out_segments, merged_segments,
         *out_segments_length * sizeof(struct SpeechSegment));
  vector_free(segments);
  vector_free(merged_segments);
}
