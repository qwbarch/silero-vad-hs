// Credits: SileroVAD's C# example:
// https://github.com/snakers4/silero-vad/blob/cb25c0c0470cf20d7f33805218831b9620dbb6a7/examples/csharp/SileroVadDetector.cs

#include "detector.h"

struct VoiceDetector *
init_detector(struct SileroModel *model, const float start_threshold,
              const float end_threshold, const int min_speech_samples,
              const int max_speech_samples, const int speech_pad_samples,
              const int min_silence_samples,
              const int min_silence_samples_at_max_speech) {
  struct VoiceDetector *vad = malloc(sizeof(struct VoiceDetector));
  vad->model = model;
  *(float *)&vad->start_threshold = start_threshold;
  *(float *)&vad->end_threshold = end_threshold;
  *(int *)&vad->min_speech_samples = min_speech_samples;
  *(int *)&vad->max_speech_samples = max_speech_samples;
  *(int *)&vad->speech_pad_samples = speech_pad_samples;
  *(int *)&vad->min_silence_samples = min_silence_samples;
  *(int *)&vad->min_silence_samples_at_max_speech =
      min_silence_samples_at_max_speech;
  return vad;
}

void release_detector(struct VoiceDetector *vad) { free(vad); }

/** Split the audio samples into segments of <bold>WINDOW_SIZE</bold> size. */
struct LinkedList *split_frames(const size_t samples_length,
                                const float *samples) {
  struct LinkedList *frames = create_list();
  for (int i = 0; i < samples_length; i += WINDOW_SIZE) {
    float *frame = malloc(WINDOW_SIZE_BYTES);
    memset(frame, 0.0f, WINDOW_SIZE_BYTES);
    int copy_bytes = WINDOW_SIZE_BYTES;
    if (i + WINDOW_SIZE_BYTES > samples_length) {
      copy_bytes = WINDOW_SIZE_BYTES - samples_length + i;
    }
    memcpy(frame, &samples[i], WINDOW_SIZE_BYTES);
    push_list(frames, frame);
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

struct LinkedList *calculate_time(struct LinkedList *original) {
  struct LinkedList *result = create_list();
  if (!original || original->length == 0)
    return result;
  int left = ((struct SpeechSegment *)original->head_node)->start_index;
  int right = ((struct SpeechSegment *)original->head_node)->end_index;
  if (original->length > 1) {
  }
  return NULL;
}

void detect_segments(struct VoiceDetector *vad, const size_t samples_length,
                     const float *samples) {
  struct LinkedList *frames = split_frames(samples_length, samples);
  reverse_list(frames);
  struct Node *current_node = frames->head_node;
  struct LinkedList *segments = create_list();
  bool triggered = false;
  int i = 0;
  int temp_end = 0;
  int previous_end = 0;
  int next_start = 0;
  struct SpeechSegment *current_segment = malloc(sizeof(struct SpeechSegment));
  reset_segment(current_segment);
  while (current_node) {
    float *frame = current_node->value;
    float probability = detect_speech(vad->model, WINDOW_SIZE, frame);

    if (probability >= vad->start_threshold && temp_end != 0) {
      temp_end = 0;
      if (next_start < previous_end) {
        next_start = WINDOW_SIZE * i;
      }
    }

    if (probability >= vad->start_threshold && !triggered) {
      triggered = true;
      current_segment->start_index = WINDOW_SIZE * i;
      continue;
    }

    if (triggered && WINDOW_SIZE * i - current_segment->start_index >
                         vad->max_speech_samples) {
      if (previous_end != 0) {
        current_segment->end_index = previous_end;
        push_list(segments, current_segment);
        current_segment = malloc(sizeof(struct SpeechSegment));
        if (next_start < previous_end) {
          triggered = false;
        } else {
          current_segment->start_index = next_start;
        }
        previous_end = 0;
        next_start = 0;
        temp_end = 0;
      } else {
        current_segment->end_index = WINDOW_SIZE * i;
        push_list(segments, current_segment);
        current_segment = malloc(sizeof(struct SpeechSegment));
        previous_end = 0;
        next_start = 0;
        temp_end = 0;
        triggered = false;
      }
    }

    if (probability < vad->end_threshold && triggered) {
      if (temp_end == 0) {
        temp_end = WINDOW_SIZE * i;
      }

      if (WINDOW_SIZE * i > temp_end > vad->min_silence_samples_at_max_speech) {
        previous_end = temp_end;
      }

      if (WINDOW_SIZE * i - temp_end < vad->min_silence_samples) {
        continue;
      } else {
        current_segment->end_index = temp_end;
        if (current_segment->end_index - current_segment->start_index >
            vad->min_silence_samples) {
          push_list(segments, current_segment);
          current_segment = malloc(sizeof(struct SpeechSegment));
        }
        reset_segment(current_segment);
        previous_end = 0;
        next_start = 0;
        temp_end = 0;
        triggered = false;
        continue;
      }
    }

    free(frame);
    current_node = pop_list(frames);
  }

  if (current_segment->start_index >= 0 &&
      WINDOW_SIZE - current_segment->start_index > vad->min_speech_samples) {
    current_segment->end_index = WINDOW_SIZE;
    push_list(segments, current_segment);
  }

  reverse_list(segments);
  i = 0;
  while (current_node) {
    current_segment = current_node->value;
    if (i == 0) {
      current_segment->start_index =
          math_max(0, current_segment->start_index - vad->speech_pad_samples);
    }
    if (i != segments->length - 1) {
      struct SpeechSegment *next_segment = segments->head_node->value;
      int silence_duration =
          next_segment->start_index - current_segment->end_index;
      if (silence_duration < vad->speech_pad_samples * 2) {
        current_segment->end_index += silence_duration / 2;
        next_segment->start_index =
            math_max(0, next_segment->start_index - silence_duration / 2);
      } else {
        current_segment->end_index = math_min(
            WINDOW_SIZE, current_segment->end_index + vad->speech_pad_samples);
        next_segment->start_index =
            math_max(0, next_segment->start_index - vad->speech_pad_samples);
      }
    } else {
      current_segment->end_index = math_min(
          WINDOW_SIZE, current_segment->end_index + vad->speech_pad_samples);
    }
    current_node = current_node->next_node;
    i++;
  }
}
