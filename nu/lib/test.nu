# test.nu
use vc.nu

export-env {
}

export def "get bundle" [--workload?:string] {
  http get "https://packy.compiler.company/bundle/core-test-data.tar.zst"
  # unpack..

  # decode taobench workload defs
  # let taobench_a = (open --raw "workload_a.json" | from json -o)
  # let taobench_o = (open --raw "workload_o.json" | from json -o)
}
