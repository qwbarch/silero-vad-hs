#include "silero_vad.h"

// 16khz sample rate can only have a window size of 512 in the v5 model.
#define WINDOW_SIZE 512
#define STATE_LENGTH 2 * 1 * 128
#define CONTEXT_SIZE 64

// Log level: Error
#define ORT_LOGGING_LEVEL 3

const int64_t sr_shape[] = {1};
const int64_t state_shape[] = {2, 1, 128};

const char *input_names[] = {"input", "state", "sr"};
const char *output_names[] = {"output", "stateN"};

struct SileroVAD *init_silero(OrtGetApiBaseFunc ortGetApiBase,
                              const char *model_path) {
  struct SileroVAD *vad = malloc(sizeof(struct SileroVAD));
  vad->api = ortGetApiBase()->GetApi(ORT_API_VERSION);
  (void)vad->api->CreateEnv(ORT_LOGGING_LEVEL, "silero_vad", &vad->env);
  (void)vad->api->CreateSessionOptions(&vad->session_options);
  (void)vad->api->SetIntraOpNumThreads(vad->session_options, 1);
  (void)vad->api->SetInterOpNumThreads(vad->session_options, 1);
  (void)vad->api->SetSessionGraphOptimizationLevel(vad->session_options,
                                                   ORT_ENABLE_ALL);
  (void)vad->api->CreateSession(vad->env, model_path, vad->session_options,
                                &vad->session);
  (void)vad->api->CreateCpuMemoryInfo(OrtArenaAllocator, OrtMemTypeCPU,
                                      &vad->memory_info);
  size_t state_bytes = STATE_LENGTH * sizeof(float);
  vad->state = malloc(state_bytes);
  memset(vad->state, 0.0f, state_bytes);
  vad->input_shape[0] = 1;
  return vad;
}

void release_silero(struct SileroVAD *vad) {
  vad->api->ReleaseEnv(vad->env);
  vad->api->ReleaseSessionOptions(vad->session_options);
  vad->api->ReleaseSession(vad->session);
  vad->api->ReleaseMemoryInfo(vad->memory_info);
  free(vad->state);
  free(vad);
}

float detect_speech(struct SileroVAD *vad, const int64_t pcm_length,
                    const char *pcm_data) {
  // Normalize the pcm data.
  size_t samples_length = pcm_length / 2;
  float *samples = malloc(samples_length * sizeof(float));
  for (int i = 0; i < samples_length; i++) {
    int16_t sample = (int16_t)(pcm_data[i * 2] | pcm_data[i * 2 + 1] << 8);
    samples[i] = (float)sample / INT16_MAX;
  }

  // Add context from previous run.
  size_t audio_length;
  float *audio_signal;
  if (vad->context == NULL) {
    audio_length = samples_length;
    audio_signal = malloc(audio_length * sizeof(float));
    vad->context = malloc(CONTEXT_SIZE * sizeof(float));
    memcpy(audio_signal, samples, samples_length * sizeof(float));
    memset(vad->context, 0, CONTEXT_SIZE * sizeof(float));
  } else {
    audio_length = samples_length + CONTEXT_SIZE;
    audio_signal = malloc(audio_length * sizeof(float));
    memcpy(audio_signal, vad->context, CONTEXT_SIZE * sizeof(float));
    memcpy(audio_signal + CONTEXT_SIZE, samples,
           samples_length * sizeof(float));
  }
  memcpy(vad->context, samples + samples_length - CONTEXT_SIZE,
         CONTEXT_SIZE * sizeof(float));
  vad->input_shape[1] = audio_length;

  // Input tensor (containing the pcm data).
  OrtValue *input_tensor = NULL;
  (void)vad->api->CreateTensorWithDataAsOrtValue(
      vad->memory_info, (float *)audio_signal, audio_length * sizeof(float),
      vad->input_shape, sizeof(vad->input_shape) / sizeof(int64_t),
      ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT, &input_tensor);
  // State tensor.
  OrtValue *state_tensor = NULL;
  (void)vad->api->CreateTensorWithDataAsOrtValue(
      vad->memory_info, vad->state, STATE_LENGTH * sizeof(float), state_shape,
      sizeof(state_shape) / sizeof(int64_t),
      ONNX_TENSOR_ELEMENT_DATA_TYPE_FLOAT, &state_tensor);

  // Sample-rate tensor (assumes 16khz).
  int64_t sample_rate[] = {16000};
  OrtValue *sr_tensor = NULL;
  (void)vad->api->CreateTensorWithDataAsOrtValue(
      vad->memory_info, sample_rate, sizeof(int64_t), sr_shape,
      sizeof(sr_shape) / sizeof(int64_t), ONNX_TENSOR_ELEMENT_DATA_TYPE_INT64,
      &sr_tensor);

  // Run inference.
  const OrtValue *input_tensors[] = {input_tensor, state_tensor, sr_tensor};
  OrtValue *output_tensors[] = {NULL, NULL};
  (void)vad->api->Run(vad->session, NULL, input_names, input_tensors,
                      sizeof(input_tensors) / sizeof(OrtValue *), output_names,
                      sizeof(output_names) / sizeof(char *), output_tensors);

  float *probabilities = NULL;
  float *state_output = NULL;

  (void)vad->api->GetTensorMutableData(output_tensors[0],
                                       (void **)&probabilities);
  (void)vad->api->GetTensorMutableData(output_tensors[1],
                                       (void **)&state_output);

  memcpy(vad->state, state_output, STATE_LENGTH * sizeof(float));

  vad->api->ReleaseValue(output_tensors[0]);
  vad->api->ReleaseValue(output_tensors[1]);
  vad->api->ReleaseValue(input_tensor);
  vad->api->ReleaseValue(state_tensor);
  vad->api->ReleaseValue(sr_tensor);

  free(audio_signal);
  free(samples);

  return probabilities[0];
}
