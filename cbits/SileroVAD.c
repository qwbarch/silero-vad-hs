#include "../lib/onnxruntime/include/onnxruntime_c_api.h"

// 16khz sample rate can only have a window size of 512 in the v5 model.
#define WINDOW_SIZE 512
#define STATE_LENGTH 2 * 1 * 128
#define CONTEXT_SIZE 64

const int64_t sr_shape[] = {1};
const int64_t state_shape[] = {2, 1, 128};

const char *input_names[] = {"input", "state", "sr"};
const char *output_names[] = {"output", "stateN"};

struct SileroVAD {
  OrtApi *api;
  OrtEnv *env;
  OrtSessionOptions *session_options;
  OrtSession *session;
  OrtMemoryInfo *memory_info;
  float *state;
  float *context;
  float *audio_signal;
  int64_t input_shape[2];
};

struct SileroVAD *init_silero(OrtApiBase *base) {
  struct SileroVAD *vad = malloc(sizeof(struct SileroVAD));
  return NULL;
}
