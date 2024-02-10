# env.nu --- Core environment vars
$env.CORE_ROOT = (pwd | path expand)
$env.STASH = $"($env.CORE_ROOT)/.stash"
$env.CARGO_TARGET_DIR = $"($env.STASH)/target"
