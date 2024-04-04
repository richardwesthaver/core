fn main() {
  let cfg = slint_build::CompilerConfiguration::new().with_style("material-dark".into());
  slint_build::compile_with_config(
    "base.slint",
    cfg
  ).unwrap();
}
