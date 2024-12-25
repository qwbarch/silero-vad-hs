#ifndef SILERO_MODEL_H
#define SILERO_MODEL_H

#include "../../lib/onnxruntime/include/onnxruntime_c_api.h"

#define WINDOW_LENGTH 512
#define WINDOW_BYTES WINDOW_LENGTH * sizeof(float)

#define SAMPLE_RATE 16000

int64_t get_window_length();
int64_t get_sample_rate();

struct SileroModel {
  const struct OrtApi *api;
  OrtEnv *env;
  OrtSessionOptions *session_options;
  OrtSession *session;
  OrtMemoryInfo *memory_info;
  float *state;
  float *context;
  float *buffer;
  int64_t input_shape[2];
  bool is_first_run;
};

struct SileroModel *load_model(OrtApiBase *(*ortGetApiBase)(), const char *model_path);
void release_model(struct SileroModel *model);
void reset_model(struct SileroModel *model);
float detect_speech(struct SileroModel *model, const float *samples);

#endif
