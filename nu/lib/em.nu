# em.nu

# Emacs utilities

export-env {
  $env.USER_EMACS_DIRECTORY = $"($env.HOME)/.emacs.d/"
  $env.ALTERNATE_EDITOR = ""
}

export extern "emacsclient" [
  files:string@"ls", # files to open
  --version(-V) # Just print version info and return
  --help(-H) # Print help
  --tty(-t) # Open a new Emacs frame on the current terminal
  --create-frame(-c) # Create a new frame instead of trying to use the current Emacs frame
  --reuse-frame(-r) # Create a new frame if none exists, otherwise use the current Emacs frame
  --frame-parameters(-F) # Set the parameters of a new frame
  --eval(-e):string # Evalue the FILE arguments as Elisp expressions
  --no-wait(-n) # Don't wait for the server to return
  --timeout(-w):int # Seconds to wait before timing out
  --quiet(-q) # Don't display messages on success
  --suppress-output(-u) # Don't display return values from the server
  --display(-d):string # Visit the file in the given display
  --parent-id # Open in parent window ID, via XEmbed
  --tramp(-T):string # Prefix to prepend to filenames sent by emacsclient for locating files remotely via Tramp
  --alternate-editor(-a):string # Editor to fallback to if the server is not
                         # running. If EDITOR is the empty string,
                         # start Emacs in daemon mode and try
                         # connecting again
]

export alias ec = if $in != null {emacsclient -c $in} else {emacsclient -c .}
export alias et = if $in != null {emacsclient -t $in} else {emacsclient -t .}

export def publish [--no-wait(-n)] {
  ^emacsclient -e $"\(progn \(load-file \"($env.USER_EMACS_DIRECTORY + lib/publish.el)\"\) \(publish\)\)" (if $no_wait { "--no-wait" } else { "" })
}
