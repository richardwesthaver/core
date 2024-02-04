# em.nu

# Emacs utilities

export-env {
  $env.USER_EMACS_DIRECTORY = $"($env.HOME)/.emacs.d/"
}

export def publish [] {
  emacsclient -a='' -e $"\(progn \(load-file \"($env.USER_EMACS_DIRECTORY + lib/publish.el)\"\) \(publish\)\)"
}
