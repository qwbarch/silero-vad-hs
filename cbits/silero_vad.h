#ifndef SILERO_VAD_H
#define SILERO_VAD_H

#include "../lib/onnxruntime/include/onnxruntime_c_api.h"
#include <stdio.h>

// Struct for the Silero VAD
struct SileroVAD {
  const struct OrtApi *api;
  OrtEnv *env;
  OrtSessionOptions *session_options;
  OrtSession *session;
  OrtMemoryInfo *memory_info;
  float *state;
  float *context;
  float *audio_signal;
  int64_t input_shape[2];
};

typedef OrtApiBase *(ORT_API_CALL *OrtGetApiBaseFunc)();

// Function prototypes
struct SileroVAD *init_silero(OrtGetApiBaseFunc ortGetApiBase, const char *model_path);
void release_silero(struct SileroVAD *vad);
float detect_speech(struct SileroVAD *vad, const int64_t pcm_length, const char *pcm_data);

#endif
