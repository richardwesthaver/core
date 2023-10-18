//! audio modules
use std::{
  fs::File,
  io::BufWriter,
  sync::{Arc, Mutex},
};

pub use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
pub use hound::{Sample, SampleFormat, WavReader, WavSpec};

/// convert between `cpal::SampleFormat` and `hound::SampleFormat`
pub fn sample_format(format: cpal::SampleFormat) -> hound::SampleFormat {
  match format {
    cpal::SampleFormat::U16 => hound::SampleFormat::Int,
    cpal::SampleFormat::I16 => hound::SampleFormat::Int,
    cpal::SampleFormat::F32 => hound::SampleFormat::Float,
  }
}

/// Return `hound::WavSpec` from `cpal::SupportedStreamconfig`
pub fn wav_spec_from_config(config: &cpal::SupportedStreamConfig) -> hound::WavSpec {
  hound::WavSpec {
    channels: config.channels() as _,
    sample_rate: config.sample_rate().0 as _,
    bits_per_sample: (config.sample_format().sample_size() * 8) as _,
    sample_format: sample_format(config.sample_format()),
  }
}

/// A thread-safe handle to the WavWriter file
type WavWriterHandle = Arc<Mutex<Option<hound::WavWriter<BufWriter<File>>>>>;

/// Write incoming data to an owned `WavWriterHandle`
pub fn write_input_data<T, U>(input: &[T], writer: &WavWriterHandle)
where
  T: cpal::Sample,
  U: cpal::Sample + hound::Sample,
{
  if let Ok(mut guard) = writer.try_lock() {
    if let Some(writer) = guard.as_mut() {
      for &sample in input.iter() {
        let sample: U = cpal::Sample::from(&sample);
        writer.write_sample(sample).ok();
      }
    }
  }
}

/// Compute the RMS of either integers or float samples.
pub fn compute_rms<S, R>(reader: &mut WavReader<R>) -> f64
where
  f64: From<S>,
  S: hound::Sample,
  R: std::io::Read,
{
  let sqr_sum = reader.samples::<S>().fold(0.0, |sqr_sum, s| {
    let sample = f64::from(s.unwrap());
    sqr_sum + sample * sample
  });
  (sqr_sum / reader.len() as f64).sqrt()
}

/// Run a max volume sinusoid through the default audio output device
pub fn run<T: cpal::Sample>(
  device: &cpal::Device,
  config: &cpal::StreamConfig,
) -> Result<(), Box<dyn std::error::Error + 'static>> {
  let sample_rate = config.sample_rate.0 as f32;
  let channels = config.channels as usize;

  // Produce a sinusoid of maximum amplitude.
  let mut sample_clock = 0f32;
  let mut next_value = move || {
    sample_clock = (sample_clock + 1.0) % sample_rate;
    (sample_clock * 440.0 * 2.0 * std::f32::consts::PI / sample_rate).sin()
  };

  let err_fn = |err| eprintln!("an error occurred on stream: {}", err);

  let stream = device.build_output_stream(
    config,
    move |data: &mut [T], _: &cpal::OutputCallbackInfo| write_data(data, channels, &mut next_value),
    err_fn,
  )?;
  stream.play()?;

  std::thread::sleep(std::time::Duration::from_millis(1000));

  Ok(())
}

/// write a stream of data
fn write_data<T>(output: &mut [T], channels: usize, next_sample: &mut dyn FnMut() -> f32)
where
  T: cpal::Sample,
{
  for frame in output.chunks_mut(channels) {
    let value: T = cpal::Sample::from::<f32>(&next_sample());
    for sample in frame.iter_mut() {
      *sample = value;
    }
  }
}

#[cfg(test)]
#[test]
fn default_device() {
  use cpal::traits::HostTrait;
  let device = cpal::default_host().default_output_device();
  assert!(&device.is_some());
  let config = device.unwrap().default_output_config();
  assert!(&config.is_ok());
}
