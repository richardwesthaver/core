//! installer.rs --- Alik Installer Example

// 

//! Code: 
use std::future::Future;

#[cfg(not(target_arch = "wasm32"))]
fn execute<F: Future<Output = ()> + Send + 'static>(f: F) {
  // this is stupid... use any executor of your choice instead
  std::thread::spawn(move || futures::executor::block_on(f));
}
#[cfg(target_arch = "wasm32")]
fn execute<F: Future<Output = ()> + 'static>(f: F) {
  wasm_bindgen_futures::spawn_local(f);
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  // sync
  let res = rfd::MessageDialog::new()
    .set_title("Msg!")
    .set_description("Description!")
    .set_buttons(rfd::MessageButtons::OkCancel)
    .show();
  println!("{}", res);  
  // async
  let task = rfd::AsyncFileDialog::new().pick_file();

  // Await somewhere else
  execute(async {
    let file = task.await;

    if let Some(file) = file {
      // If you are on native platform you can just get the path
      #[cfg(not(target_arch = "wasm32"))]
      println!("{:?}", file.path());
      // on wasm just file.read().await;
    }
  });
  eframe::run_native("alik_installer_example",
                     eframe::NativeOptions::default(),
                     Box::new(|cc| Box::new(alik_ui::AlikApp::new(cc)))).unwrap();
  Ok(())
}
