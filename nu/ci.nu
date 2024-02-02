# ci.nu --- Nushell CI config file
use nu/lib/ *
# default LISP command used inside boxes
$env.LISP = "/usr/local/bin/sbcl --core /usr/local/lib/sbcl/prelude.core"
$env.config = {
  # true or false to enable or disable the welcome banner at startup
  show_banner: false
  ls: {
    use_ls_colors: false # use the LS_COLORS environment variable to colorize output
    clickable_links: false # enable or disable clickable links. Your terminal has to support links.
  }
  rm: {
    always_trash: false # always act as if -t was given. Can be overridden with -p
  }
  table: {
    mode: basic # basic, compact, compact_double, light, thin, with_love, rounded, reinforced, heavy, none, other
    index_mode: always # "always" show indexes, "never" show indexes, "auto" = show indexes when a table has "index" column
    show_empty: true # show 'empty list' and 'empty record' placeholders for command output
    trim: {
      methodology: wrapping # wrapping or truncating
      wrapping_try_keep_words: true # A strategy used by the 'wrapping' methodology
      truncating_suffix: "..." # A suffix used by the 'truncating' methodology
    }
  }

  use_grid_icons: false
  float_precision: 2 # the precision for displaying floats in tables
  use_ansi_coloring: false
  edit_mode: emacs # emacs, vi
  shell_integration: true # enables terminal markers and a workaround to arrow keys stop working issue
  hooks: {
    pre_prompt: [{||
      null
    }]
    pre_execution: [{||
      null
    }]
    env_change: {
      PWD: [{|before, after|
        null
      }]
    }
    display_output: {||
      if (term size).columns >= 100 { table -e } else { table }
    }
    command_not_found: {||
      null
    }
  }
}
