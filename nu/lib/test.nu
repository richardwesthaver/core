# test.nu
use std log
use vc.nu

export const WORKLOADS = [
  "taobench-a", "taobench-o", 
  "1nrc", "1nrc-10", "1nrc-20", "1nrc-40", "1nrc-80", 
  "1brc", "1trc"
]

export def "get bundle" [workload?:string] {
  http get "https://packy.compiler.company/bundle/core-test-data.tar.zst"
  # unpack..

  # decode taobench workload defs
  # let taobench_a = (open --raw "workload_a.json" | from json -o)
  # let taobench_o = (open --raw "workload_o.json" | from json -o)

  # prepare dataframe benchmarks
}

# Generate a N*10M row data bundle by sampling randomly from the
# 1TRC dataset.
export def "bundle 1nrc" [n:int=1, --dir:directory="."] {
  let 1trc_dir = $"/mnt/y/data/sets/1trc"
  let ftotal = (ls $1trc_dir | length);
  let out = ($"($dir)/1nrc-($n)" | path expand)
  mkdir $out
  cd $out
  (0..<$n | par-each {cp $"($1trc_dir)/measurements-(random int ..$ftotal).parquet" ./})
  tar -I "zstd -T0" -cf $"($out).tar.zst" ./*
  # rm -rf $out
}

# Single-threaded aggregation function for 1TRC-based data.
export def agg_chunk [files,idx,inc] {
  let start = ($idx * $inc);
  let last = ([($start + $inc) ($files | length)] | math min);
  let slice = ($files | range $start..$last);
  if ($slice | is-empty) { return } else {
  log info $"chunk: [($idx)] ($start)..($last)"
  $slice
    | each {|it| dfr open $it}
    | reduce {|a,b| $a | dfr append $b --col}
    | dfr group-by station
    | dfr agg [
      (dfr col measure | dfr min | dfr as min)
      (dfr col measure | dfr max | dfr as max)
      (dfr col measure | dfr sum | dfr as sum)
      (dfr col measure | dfr count | dfr as count)
    ]
  }
}

# Perform a benchmark using the 1-Trillion Row Challenge dataset.
#
# src: https://github.com/coiled/1trc
#
# Input must be a directory containing files of the following form:
# measurements-N.parquet where N is a uint below $max.
export def "bench 1trc" [
  --output:string="1trc.txt"
  --batch:int=4
  --threads:int=4
] {
  let d = ($in | path expand)
  log info $"Starting 1TRC benchmark in ($d)..."
  let now = ((date now) | format date "%Y-%m-%d.%H:%M:%S")
  let dir = (ls $d | select name | values).0
  let fs = ($dir | length)
  let batches = $fs // $batch
  log info $"($fs) files"
  log info $"batch_count: ($batches)"
  timeit {
    let df = (0..<$batches | par-each -t $threads { |idx|
      agg_chunk $dir $idx $batch
    } | reduce {|a,b| $a | dfr append $b --col}
    | dfr group-by station
    | dfr agg [
      (dfr col min | dfr min)
      (dfr col max | dfr max)
      (dfr col sum | dfr sum)
      (dfr col count | dfr sum)
    ]
    | dfr sort-by station
    );
    $df | dfr drop count sum
    | dfr with-column (
      $df | dfr select sum count | dfr into-nu | par-each { $in.sum / $in.count } | dfr into-df
    ) --name mean
    | dfr collect | print $in
  } | log info $"run @ ($now) completed in ($in)\n"
}

# Perform a benchmark using (N * 10,000,000) Rows.
export def "bench 1nrc" [
  n:int=100
  --output:string="1nrc.parquet"
  --batch:int=4
  --threads:int=4
] {
  let d = ($in | path expand);
  log info $"Starting 1NRC benchmark in ($d)..."
  let now = ((date now) | format date "%Y-%m-%d.%H:%M:%S");
  let ftotal = (ls $d | length);
  let dir = (0..<$n | par-each {$"($d)/measurements-(random int 0..<$ftotal).parquet"});
  let fs = ($dir | length);
  let batches = $fs // $batch;
  log info $"($fs) files"
  log info $"batch_count: ($batches)"
  timeit {
    let df = (0..<$batches | par-each -t $threads { |idx|
      agg_chunk $dir $idx $batch
    } | reduce {|a,b| $a | dfr append $b --col}
    | dfr group-by station
    | dfr agg [
      (dfr col min | dfr min)
      (dfr col max | dfr max)
      (dfr col sum | dfr sum)
      (dfr col count | dfr sum)
    ]
    | dfr sort-by station
    );
    $df | dfr drop count sum 
    | dfr with-column (
      $df | dfr select sum count | dfr into-nu | par-each { $in.sum / $in.count } | dfr into-df
    ) --name mean
    | dfr collect | print $in
  } | log info $"run @ ($now) completed in ($in) where N = ($n * 10000000)\n"
}
