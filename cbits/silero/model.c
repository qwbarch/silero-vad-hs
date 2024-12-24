#include "model.h"

// 16khz sample rate can only have a window size of 512 in the v5 model.
#define STATE_LENGTH 2 * 1 * 128
#define CONTEXT_SIZE 64

// Log level: Error
#define ORT_LOGGING_LEVEL 3

const int64_t sr_shape[] = {1};
const int64_t state_shape[] = {2, 1, 128};

const char *input_names[] = {"input", "state", "sr"};
const char *output_names[] = {"output", "stateN"};

float get_window_size() { return WINDOW_SIZE; }

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
  size_t state_bytes = STATE_LENGTH * sizeof(float);
  model->state = malloc(state_bytes);
  memset(model->state, 0.0f, state_bytes);
  model->input_shape[0] = 1;
  model->context = NULL;
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

float detect_speech(struct SileroModel *model, const size_t samples_length,
                    const float *samples) {
  // Add context from previous run.
  size_t audio_length;
  float *audio_signal;
  if (!model->context) {
    audio_length = samples_length;
    audio_signal = malloc(audio_length * sizeof(float));
    model->context = malloc(CONTEXT_SIZE * sizeof(float));
    memcpy(audio_signal, samples, samples_length * sizeof(float));
    memset(model->context, 0, CONTEXT_SIZE * sizeof(float));
  } else {
    audio_length = samples_length + CONTEXT_SIZE;
    audio_signal = malloc(audio_length * sizeof(float));
    memcpy(audio_signal, model->context, CONTEXT_SIZE * sizeof(float));
    memcpy(audio_signal + CONTEXT_SIZE, samples,
           samples_length * sizeof(float));
  }
  memcpy(model->context, samples + samples_length - CONTEXT_SIZE,
         CONTEXT_SIZE * sizeof(float));
  model->input_shape[1] = audio_length;

  // Input tensor (containing the pcm data).
  OrtValue *input_tensor = NULL;
  (void)model->api->CreateTensorWithDataAsOrtValue(
      model->memory_info, (float *)audio_signal, audio_length * sizeof(float),
      model->input_shape, sizeof(model->input_shape) / sizeof(int64_t),
      ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT, &input_tensor);
  // State tensor.
  OrtValue *state_tensor = NULL;
  (void)model->api->CreateTensorWithDataAsOrtValue(
      model->memory_info, model->state, STATE_LENGTH * sizeof(float),
      state_shape, sizeof(state_shape) / sizeof(int64_t),
      ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT, &state_tensor);

  // Sample-rate tensor (assumes 16khz).
  int64_t sample_rate[] = {16000};
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
  float *state_output = NULL;

  (void)model->api->GetTensorMutableData(output_tensors[0],
                                         (void **)&probabilities);
  (void)model->api->GetTensorMutableData(output_tensors[1],
                                         (void **)&state_output);

  memcpy(model->state, state_output, STATE_LENGTH * sizeof(float));

  model->api->ReleaseValue(output_tensors[0]);
  model->api->ReleaseValue(output_tensors[1]);
  model->api->ReleaseValue(input_tensor);
  model->api->ReleaseValue(state_tensor);
  model->api->ReleaseValue(sr_tensor);

  free(audio_signal);

  return probabilities[0];
}
