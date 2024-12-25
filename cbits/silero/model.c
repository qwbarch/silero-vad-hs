#include "model.h"

// 16khz sample rate can only have a window size of 512 in the v5 model.
#define STATE_LENGTH 2 * 1 * 128
#define STATE_BYTES STATE_LENGTH * sizeof(float)

#define CONTEXT_LENGTH 64
#define CONTEXT_BYTES CONTEXT_LENGTH * sizeof(float)

#define BUFFER_LENGTH WINDOW_LENGTH + CONTEXT_LENGTH
#define BUFFER_BYTES WINDOW_BYTES + CONTEXT_BYTES

// Log level: Error
#define ORT_LOGGING_LEVEL 3

const int64_t sr_shape[] = {1};
const int64_t state_shape[] = {2, 1, 128};

const char *input_names[] = {"input", "state", "sr"};
const char *output_names[] = {"output", "stateN"};

float get_window_size() { return WINDOW_LENGTH; }

struct SileroModel *load_model(OrtApiBase *(*ortGetApiBase)(),
                               const char *model_path) {
  struct SileroModel *model = malloc(sizeof(struct SileroModel));
  model->api = ortGetApiBase()->GetApi(ORT_API_VERSION);
  (void)model->api->CreateEnv(ORT_LOGGING_LEVEL, "silero_model", &model->env);
  (void)model->api->CreateSessionOptions(&model->session_options);
  (void)model->api->SetIntraOpNumThreads(model->session_options, 1);
  (void)model->api->SetInterOpNumThreads(model->session_options, 1);
  (void)model->api->SetSessionGraphOptimizationLevel(model->session_options,
                                                     ORT_ENABLE_ALL);
  (void)model->api->CreateSession(model->env, model_path,
                                  model->session_options, &model->session);
  (void)model->api->CreateCpuMemoryInfo(OrtArenaAllocator, OrtMemTypeCPU,
                                        &model->memory_info);
  model->state = calloc(STATE_LENGTH, STATE_BYTES);
  model->buffer = calloc(BUFFER_LENGTH, BUFFER_BYTES);
  model->context = calloc(CONTEXT_LENGTH, CONTEXT_BYTES);
  model->input_shape[0] = 1;
  model->is_first_run = true;
  return model;
}

void release_model(struct SileroModel *model) {
  model->api->ReleaseEnv(model->env);
  model->api->ReleaseSessionOptions(model->session_options);
  model->api->ReleaseSession(model->session);
  model->api->ReleaseMemoryInfo(model->memory_info);
  free(model->state);
  free(model);
}

void reset_model(struct SileroModel *model) {
  // TODO
  // memset(model->state, 0, STATE_LENGTH * sizeof(float));
}

float detect_speech(struct SileroModel *model, const float *samples) {
  // Add context from previous run.
  if (model->is_first_run) {
    model->is_first_run = false;
    model->input_shape[1] = WINDOW_LENGTH;
    memcpy(model->buffer, samples, WINDOW_BYTES);
  } else {
    model->input_shape[1] = BUFFER_LENGTH;
    memcpy(model->buffer, model->context, CONTEXT_BYTES);
    memcpy(model->buffer + CONTEXT_LENGTH, samples, WINDOW_BYTES);
  }
  // Save context for next run.
  memcpy(model->context, samples + WINDOW_LENGTH - CONTEXT_LENGTH,
         CONTEXT_BYTES);

  // Input tensor (containing the pcm data).
  OrtValue *input_tensor = NULL;
  (void)model->api->CreateTensorWithDataAsOrtValue(
      model->memory_info, model->buffer, model->input_shape[1] * sizeof(float),
      model->input_shape, sizeof(model->input_shape) / sizeof(int64_t),
      ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT, &input_tensor);
  // State tensor.
  OrtValue *state_tensor = NULL;
  (void)model->api->CreateTensorWithDataAsOrtValue(
      model->memory_info, model->state, STATE_BYTES, state_shape,
      sizeof(state_shape) / sizeof(int64_t),
      ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT, &state_tensor);

  // Sample-rate tensor (assumes 16khz).
  int64_t sample_rate[] = {SAMPLE_RATE};
  OrtValue *sr_tensor = NULL;
  (void)model->api->CreateTensorWithDataAsOrtValue(
      model->memory_info, sample_rate, sizeof(int64_t), sr_shape,
      sizeof(sr_shape) / sizeof(int64_t), ONNX_TENSOR_ELEMENT_DATA_TYPE_INT64,
      &sr_tensor);

  // Run inference.
  const OrtValue *input_tensors[] = {input_tensor, state_tensor, sr_tensor};
  OrtValue *output_tensors[] = {NULL, NULL};
  (void)model->api->Run(model->session, NULL, input_names, input_tensors,
                        sizeof(input_tensors) / sizeof(OrtValue *),
                        output_names, sizeof(output_names) / sizeof(char *),
                        output_tensors);

  float *probabilities = NULL;
  float *state_n = NULL;

  (void)model->api->GetTensorMutableData(output_tensors[0],
                                         (void **)&probabilities);
  (void)model->api->GetTensorMutableData(output_tensors[1], (void **)&state_n);

  memcpy(model->state, state_n, STATE_BYTES);

  model->api->ReleaseValue(output_tensors[0]);
  model->api->ReleaseValue(output_tensors[1]);
  model->api->ReleaseValue(input_tensor);
  model->api->ReleaseValue(state_tensor);
  model->api->ReleaseValue(sr_tensor);

  return probabilities[0];
}
