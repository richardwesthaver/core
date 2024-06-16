// When compiling natively:
#[cfg(not(target_arch = "wasm32"))]
fn main() -> eframe::Result<()> {
  // env_logger::init(); // Log to stderr (if you run with `RUST_LOG=debug`).

  let mut native_options = eframe::NativeOptions {
    viewport: egui::ViewportBuilder::default()
      .with_inner_size([400.0, 300.0])
      .with_min_inner_size([300.0, 220.0])
      .with_taskbar(true)
      .with_decorations(true),
    // .with_icon(
    // // NOTE: Adding an icon is optional
    // eframe::icon_data::from_png_bytes(&include_bytes!("../assets/icon-256.
    // png")[..]) .expect("Failed to load icon"),
    ..Default::default()
  };
  native_options.default_theme = eframe::Theme::Dark;
  eframe::run_native(
    "alik",
    native_options,
    Box::new(|cc| Box::new(alik_ui::AlikApp::new(cc))),
  )
}

// When compiling to web using trunk:
#[cfg(target_arch = "wasm32")]
fn main() {
  // Redirect `log` message to `console.log` and friends:
  eframe::WebLogger::init(log::LevelFilter::Debug).ok();

  let web_options = eframe::WebOptions::default();

  wasm_bindgen_futures::spawn_local(async {
    eframe::WebRunner::new()
      .start(
        "app_canvas", // hardcode it
        web_options,
        Box::new(|cc| Box::new(alik_ui::AlikApp::new(cc))),
      )
      .await
      .expect("failed to start eframe");
  });
}
