# gst.nu

# GStreamer module for Nushell

export def test [] { gst-launch-1.0 audiotestsrc ! autoaudiosink videotestsrc ! autovideosink }

export def "test video" [] {
  gst-launch-1.0 videotestsrc ! videoconvert ! autovideosink
}

export def "test audio" [] {
  gst-launch-1.0 audiotestsrc ! autoaudiosink
}

export def "play" [file: string] {
  gst-play-1.0 $file
}

export def "play dir" [dir?: string] {
  if $dir != null {cd $dir}
  ls | where type == file | each { |it|
    play $it.name
  }
}

export def "play mkv video" [file: string] {
  let location = $"location=($file)"
  gst-launch-1.0 filesrc $location ! matroskademux ! vp9dec ! videoconvert ! videoscale ! autovideosink
}
