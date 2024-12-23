#ifndef SILERO_MODEL_H
#define SILERO_MODEL_H

#include "../../lib/onnxruntime/include/onnxruntime_c_api.h"

#define WINDOW_SIZE 512
#define WINDOW_SIZE_BYTES WINDOW_SIZE * sizeof(float)

struct SileroModel {
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

struct SileroModel *load_model(OrtApiBase *(*ortGetApiBase)(), const char *model_path);
void release_model(struct SileroModel *model);
float detect_speech(struct SileroModel *model, const size_t sample_length, const float *samples);

#endif
