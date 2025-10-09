;;; modus.lisp --- Modus Operandi and Vivendi color themes

;; Credits to Protesilaos for showing us that themes can be functional and beautiful.

;;; Code:
(in-package :obj/color)
(make-palette :modus-operandi
  ;; Basic values
  :bg-main          "ffffff"
  :bg-dim           "f2f2f2"
  :fg-main          "000000"
  :fg-dim           "595959"
  :fg-alt           "193668"
  :bg-active        "c4c4c4"
  :bg-inactive      "e0e0e0"
  :border           "9f9f9f"
  ;; Common accent foregrounds
  :red             "a60000"
  :red-warmer      "972500"
  :red-cooler      "a0132f"
  :red-faint       "7f0000"
  :red-intense     "d00000"
  :green           "006800"
  :green-warmer    "316500"
  :green-cooler    "00663f"
  :green-faint     "2a5045"
  :green-intense   "008900"
  :yellow          "6f5500"
  :yellow-warmer   "884900"
  :yellow-cooler   "7a4f2f"
  :yellow-faint    "624416"
  :yellow-intense  "808000"
  :blue            "0031a9"
  :blue-warmer     "3548cf"
  :blue-cooler     "0000b0"
  :blue-faint      "003497"
  :blue-intense    "0000ff"
  :magenta         "721045"
  :magenta-warmer  "8f0075"
  :magenta-cooler  "531ab6"
  :magenta-faint   "7c318f"
  :magenta-intense "dd22dd"
  :cyan            "005e8b"
  :cyan-warmer     "3f578f"
  :cyan-cooler     "005f5f"
  :cyan-faint      "005077"
  :cyan-intense    "008899"
  ;; Uncommon accent foregrounds
  :rust       "8a290f"
  :gold       "80601f"
  :olive      "56692d"
  :slate      "2f3f83"
  :indigo     "4a3a8a"
  :maroon     "731c52"
  :pink       "7b435c"
  ;; Common accent backgrounds
  :bg-red-intense     "ff8f88"
  :bg-green-intense   "8adf80"
  :bg-yellow-intense  "f3d000"
  :bg-blue-intense    "bfc9ff"
  :bg-magenta-intense "dfa0f0"
  :bg-cyan-intense    "a4d5f9"

  :bg-red-subtle      "ffcfbf"
  :bg-green-subtle    "b3fabf"
  :bg-yellow-subtle   "fff576"
  :bg-blue-subtle     "ccdfff"
  :bg-magenta-subtle  "ffddff"
  :bg-cyan-subtle     "bfefff"

  :bg-red-nuanced     "ffe8e8"
  :bg-green-nuanced   "e0f6e0"
  :bg-yellow-nuanced  "f8f0d0"
  :bg-blue-nuanced    "ecedff"
  :bg-magenta-nuanced "f8e6f5"
  :bg-cyan-nuanced    "e0f2fa"
  ;; Uncommon accent background and foreground pairs
  :bg-clay     "f1c8b5"
  :fg-clay     "63192a"

  :bg-ochre    "f0e3c0"
  :fg-ochre    "573a30"

  :bg-lavender "dfcdfa"
  :fg-lavender "443379"

  :bg-sage     "c0e7d4"
  :fg-sage     "124b41"
  ;; Graphs
  :bg-graph-red-0     "ef7969"
  :bg-graph-red-1     "ffaab4"
  :bg-graph-green-0   "45c050"
  :bg-graph-green-1   "75ef30"
  :bg-graph-yellow-0  "ffcf00"
  :bg-graph-yellow-1  "f9ff00"
  :bg-graph-blue-0    "7f90ff"
  :bg-graph-blue-1    "a6c0ff"
  :bg-graph-magenta-0 "e07fff"
  :bg-graph-magenta-1 "fad0ff"
  :bg-graph-cyan-0    "70d3f0"
  :bg-graph-cyan-1    "afefff"
  ;; Special purpose
  :bg-completion       "c0deff"
  :bg-hover            "b2e4dc"
  :bg-hover-secondary  "f5d0a0"
  :bg-hl-line          "dae5ec"
  :bg-region           "bdbdbd"
  :fg-region           "000000"

  :bg-mode-line-active        "c8c8c8"
  :fg-mode-line-active        "000000"
  :border-mode-line-active    "5a5a5a"
  :bg-mode-line-inactive      "e6e6e6"
  :fg-mode-line-inactive      "585858"
  :border-mode-line-inactive  "a3a3a3"

  :modeline-err     "7f0000"
  :modeline-warning "5f0070"
  :modeline-info    "002580"

  :bg-tab-bar      "dfdfdf"
  :bg-tab-current  "ffffff"
  :bg-tab-other    "c2c2c2"
  ;; Diffs
  :bg-added           "c1f2d1"
  :bg-added-faint     "d8f8e1"
  :bg-added-refine    "aee5be"
  :bg-added-fringe    "6cc06c"
  :fg-added           "005000"
  :fg-added-intense   "006700"

  :bg-changed         "ffdfa9"
  :bg-changed-faint   "ffefbf"
  :bg-changed-refine  "fac090"
  :bg-changed-fringe  "d7c20a"
  :fg-changed         "553d00"
  :fg-changed-intense "655000"

  :bg-removed         "ffd8d5"
  :bg-removed-faint   "ffe9e9"
  :bg-removed-refine  "f3b5af"
  :bg-removed-fringe  "d84a4f"
  :fg-removed         "8f1313"
  :fg-removed-intense "aa2222"

  :bg-diff-context    "f3f3f3"
  ;; Paren match
  :bg-paren-match        "5fcfff"
  :fg-paren-match        :fg-main
  :bg-paren-expression   "efd3f5"
  ;; :underline-paren-match :unspecified
  ;; Mappings

  ;; General mappings
  :fringe :bg-dim
  :cursor :fg-main

  :keybind :blue-cooler
  :name :magenta
  :identifier :yellow-cooler

  :err :red
  :warning :yellow-warmer
  :info :cyan-cooler

  :underline-err :red-intense
  :underline-warning :yellow-intense
  :underline-note :cyan-intense

  :bg-prominent-err :bg-red-intense
  :fg-prominent-err :fg-main
  :bg-prominent-warning :bg-yellow-intense
  :fg-prominent-warning :fg-main
  :bg-prominent-note :bg-cyan-intense
  :fg-prominent-note :fg-main

  :bg-active-argument :bg-yellow-nuanced
  :fg-active-argument :yellow-warmer
  :bg-active-value :bg-cyan-nuanced
  :fg-active-value :cyan-warmer
  ;; Code mappings
  :bracket :fg-main
  :builtin :magenta-warmer
  :comment :fg-dim
  :constant :blue-cooler
  :delimiter :fg-main
  :docmarkup :magenta-faint
  :docstring :green-faint
  :fnname :magenta
  :keyword :magenta-cooler
  :number :fg-main
  :operator :fg-main
  :preprocessor :red-cooler
  :property :cyan
  :punctuation :fg-main
  :rx-backslash :magenta
  :rx-construct :green-cooler
  :string :blue-warmer
  :type :cyan-cooler
  :variable :cyan
  ;; Accent mappings
  :accent-0 :blue
  :accent-1 :magenta-warmer
  :accent-2 :cyan
  :accent-3 :red
  ;; Button mappings
  :fg-button-active :fg-main
  :fg-button-inactive :fg-dim
  :bg-button-active :bg-active
  :bg-button-inactive :bg-dim
  ;; Completion mappings
  :fg-completion-match-0 :blue
  :fg-completion-match-1 :magenta-warmer
  :fg-completion-match-2 :cyan
  :fg-completion-match-3 :red
  ;; :bg-completion-match-0 :unspecified
  ;; :bg-completion-match-1 :unspecified
  ;; :bg-completion-match-2 :unspecified
  ;; :bg-completion-match-3 :unspecified
  ;; Date mappings
  :date-common :cyan
  :date-deadline :red-cooler
  :date-deadline-subtle :red-faint
  :date-event :fg-alt
  :date-holiday :red
  :date-holiday-other :blue
  :date-now :fg-main
  :date-range :fg-alt
  :date-scheduled :yellow
  :date-scheduled-subtle :yellow-faint
  :date-weekday :cyan
  :date-weekend :magenta
  ;; Line number mappings
  :fg-line-number-inactive :fg-dim
  :fg-line-number-active :fg-main
  :bg-line-number-inactive :bg-dim
  :bg-line-number-active :bg-active
  ;; Link mappings
  :fg-link :blue-warmer
  ;; :bg-link :unspecified
  :underline-link :blue-warmer

  :fg-link-symbolic :cyan
  ;; :bg-link-symbolic :unspecified
  :underline-link-symbolic :cyan

  :fg-link-visited :magenta
  ;; :bg-link-visited :unspecified
  :underline-link-visited :magenta
  ;; Mail mappings
  :mail-cite-0 :blue-faint
  :mail-cite-1 :yellow-warmer
  :mail-cite-2 :cyan-cooler
  :mail-cite-3 :red-cooler
  :mail-part :cyan
  :mail-recipient :magenta-cooler
  :mail-subject :magenta-warmer
  :mail-other :magenta-faint
  ;; Mark mappings
  :bg-mark-delete :bg-red-subtle
  :fg-mark-delete :red
  :bg-mark-select :bg-cyan-subtle
  :fg-mark-select :cyan
  :bg-mark-other :bg-yellow-subtle
  :fg-mark-other :yellow
  ;; Prompt mappings
  :fg-prompt :cyan-cooler
  ;; :bg-prompt :unspecified
  ;; Prose mappings
  :bg-prose-block-delimiter :bg-dim
  :fg-prose-block-delimiter :fg-dim
  :bg-prose-block-contents :bg-dim

  ;; :bg-prose-code :unspecified
  :fg-prose-code :cyan-cooler

  ;; :bg-prose-macro :unspecified
  :fg-prose-macro :magenta-cooler

  ;; :bg-prose-verbatim :unspecified
  :fg-prose-verbatim :magenta-warmer

  :prose-done :green
  :prose-todo :red

  :prose-metadata :fg-dim
  :prose-metadata-value :fg-alt

  :prose-table :fg-alt
  :prose-table-formula :magenta-warmer

  :prose-tag :magenta-faint
  ;; Rainbow mappings
  :rainbow-0 :fg-main
  :rainbow-1 :magenta-intense
  :rainbow-2 :cyan-intense
  :rainbow-3 :red-warmer
  :rainbow-4 :yellow-intense
  :rainbow-5 :magenta-cooler
  :rainbow-6 :green-intense
  :rainbow-7 :blue-warmer
  :rainbow-8 :magenta-warmer
  ;; Search mappings
  :bg-search-current :bg-yellow-intense
  :bg-search-lazy :bg-cyan-intense
  :bg-search-replace :bg-red-intense

  :bg-search-rx-group-0 :bg-blue-intense
  :bg-search-rx-group-1 :bg-green-intense
  :bg-search-rx-group-2 :bg-red-subtle
  :bg-search-rx-group-3 :bg-magenta-subtle
  ;; Space mappings
  ;; :bg-space :unspecified
  :fg-space :border
  :bg-space-err :bg-red-intense
  ;; Terminal mappings
  :bg-term-black           "000000"
  :fg-term-black           "000000"
  :bg-term-black-bright    "595959"
  :fg-term-black-bright    "595959"

  :bg-term-red :red
  :fg-term-red :red
  :bg-term-red-bright :red-warmer
  :fg-term-red-bright :red-warmer
  :bg-term-green :green
  :fg-term-green           :green
  :bg-term-green-bright    :green-cooler
  :fg-term-green-bright    :green-cooler

  :bg-term-yellow          :yellow
  :fg-term-yellow          :yellow
  :bg-term-yellow-bright   :yellow-warmer
  :fg-term-yellow-bright   :yellow-warmer

  :bg-term-blue            :blue
  :fg-term-blue            :blue
  :bg-term-blue-bright     :blue-warmer
  :fg-term-blue-bright     :blue-warmer

  :bg-term-magenta         :magenta
  :fg-term-magenta         :magenta
  :bg-term-magenta-bright  :magenta-cooler
  :fg-term-magenta-bright  :magenta-cooler

  :bg-term-cyan            :cyan
  :fg-term-cyan            :cyan
  :bg-term-cyan-bright     :cyan-cooler
  :fg-term-cyan-bright     :cyan-cooler

  :bg-term-white           "a6a6a6"
  :fg-term-white           "a6a6a6"
  :bg-term-white-bright    "ffffff"
  :fg-term-white-bright    "ffffff"
  ;; Heading mappings
  :fg-heading-0 :cyan-cooler
  :fg-heading-1 :fg-main
  :fg-heading-2 :yellow-faint
  :fg-heading-3 :fg-alt
  :fg-heading-4 :magenta
  :fg-heading-5 :green-faint
  :fg-heading-6 :red-faint
  :fg-heading-7 :cyan-warmer
  :fg-heading-8 :fg-dim
  ;; :bg-heading-0 :unspecified
  ;; :bg-heading-1 :unspecified
  ;; :bg-heading-2 :unspecified
  ;; :bg-heading-3 :unspecified
  ;; :bg-heading-4 :unspecified
  ;; :bg-heading-5 :unspecified
  ;; :bg-heading-6 :unspecified
  ;; :bg-heading-7 :unspecified
  ;; :bg-heading-8 :unspecified
  ;; :overline-heading-0 :unspecified
  ;; :overline-heading-1 :unspecified
  ;; :overline-heading-2 :unspecified
  ;; :overline-heading-3 :unspecified
  ;; :overline-heading-4 :unspecified
  ;; :overline-heading-5 :unspecified
  ;; :overline-heading-6 :unspecified
  ;; :overline-heading-7 :unspecified
  ;; :overline-heading-8 :unspecified
  )


(make-palette :modus-vivendi
  ;; Basic values
  :bg-main          "000000"
  :bg-dim           "1e1e1e"
  :fg-main          "ffffff"
  :fg-dim           "989898"
  :fg-alt           "c6daff"
  :bg-active        "535353"
  :bg-inactive      "303030"
  :border           "646464"
  ;; Common accent foregrounds
  :red             "ff5f59"
  :red-warmer      "ff6b55"
  :red-cooler      "ff7f86"
  :red-faint       "ff9580"
  :red-intense     "ff5f5f"
  :green           "44bc44"
  :green-warmer    "70b900"
  :green-cooler    "00c06f"
  :green-faint     "88ca9f"
  :green-intense   "44df44"
  :yellow          "d0bc00"
  :yellow-warmer   "fec43f"
  :yellow-cooler   "dfaf7a"
  :yellow-faint    "d2b580"
  :yellow-intense  "efef00"
  :blue            "2fafff"
  :blue-warmer     "79a8ff"
  :blue-cooler     "00bcff"
  :blue-faint      "82b0ec"
  :blue-intense    "338fff"
  :magenta         "feacd0"
  :magenta-warmer  "f78fe7"
  :magenta-cooler  "b6a0ff"
  :magenta-faint   "caa6df"
  :magenta-intense "ff66ff"
  :cyan            "00d3d0"
  :cyan-warmer     "4ae2f0"
  :cyan-cooler     "6ae4b9"
  :cyan-faint      "9ac8e0"
  :cyan-intense    "00eff0"
  ;; Uncommon accent foregrounds
  :rust       "db7b5f"
  :gold       "c0965b"
  :olive      "9cbd6f"
  :slate      "76afbf"
  :indigo     "9099d9"
  :maroon     "cf7fa7"
  :pink       "d09dc0"
  ;; Common accent backgrounds
  :bg-red-intense     "9d1f1f"
  :bg-green-intense   "2f822f"
  :bg-yellow-intense  "7a6100"
  :bg-blue-intense    "1640b0"
  :bg-magenta-intense "7030af"
  :bg-cyan-intense    "2266ae"

  :bg-red-subtle      "620f2a"
  :bg-green-subtle    "00422a"
  :bg-yellow-subtle   "4a4000"
  :bg-blue-subtle     "242679"
  :bg-magenta-subtle  "552f5f"
  :bg-cyan-subtle     "004065"

  :bg-red-nuanced     "3a0c14"
  :bg-green-nuanced   "092f1f"
  :bg-yellow-nuanced  "381d0f"
  :bg-blue-nuanced    "12154a"
  :bg-magenta-nuanced "2f0c3f"
  :bg-cyan-nuanced    "042837"
  ;; Uncommon accent background and foreground pairs
  :bg-clay     "49191a"
  :fg-clay     "f1b090"

  :bg-ochre    "462f20"
  :fg-ochre    "e0d09c"

  :bg-lavender "38325c"
  :fg-lavender "dfc0f0"

  :bg-sage     "143e32"
  :fg-sage     "c3e7d4"
  ;; Graphs
  :bg-graph-red-0     "b52c2c"
  :bg-graph-red-1     "702020"
  :bg-graph-green-0   "0fed00"
  :bg-graph-green-1   "007800"
  :bg-graph-yellow-0  "f1e00a"
  :bg-graph-yellow-1  "b08940"
  :bg-graph-blue-0    "2fafef"
  :bg-graph-blue-1    "1f2f8f"
  :bg-graph-magenta-0 "bf94fe"
  :bg-graph-magenta-1 "5f509f"
  :bg-graph-cyan-0    "47dfea"
  :bg-graph-cyan-1    "00808f"
  ;; Special purpose
  :bg-completion       "2f447f"
  :bg-hover            "45605e"
  :bg-hover-secondary  "654a39"
  :bg-hl-line          "2f3849"
  :bg-region           "5a5a5a"
  :fg-region           "ffffff"

  :bg-mode-line-active        "505050"
  :fg-mode-line-active        "ffffff"
  :border-mode-line-active    "959595"
  :bg-mode-line-inactive      "2d2d2d"
  :fg-mode-line-inactive      "969696"
  :border-mode-line-inactive  "606060"

  :modeline-err     "ffa9bf"
  :modeline-warning "dfcf43"
  :modeline-info    "9fefff"

  :bg-tab-bar      "313131"
  :bg-tab-current  "000000"
  :bg-tab-other    "545454"
  ;; Diffs
  :bg-added           "00381f"
  :bg-added-faint     "002910"
  :bg-added-refine    "034f2f"
  :bg-added-fringe    "237f3f"
  :fg-added           "a0e0a0"
  :fg-added-intense   "80e080"

  :bg-changed         "363300"
  :bg-changed-faint   "2a1f00"
  :bg-changed-refine  "4a4a00"
  :bg-changed-fringe  "8a7a00"
  :fg-changed         "efef80"
  :fg-changed-intense "c0b05f"

  :bg-removed         "4f1119"
  :bg-removed-faint   "380a0f"
  :bg-removed-refine  "781a1f"
  :bg-removed-fringe  "b81a1f"
  :fg-removed         "ffbfbf"
  :fg-removed-intense "ff9095"

  :bg-diff-context    "1a1a1a"
  ;; Paren match
  :bg-paren-match        "2f7f9f"
  :fg-paren-match        :fg-main
  :bg-paren-expression   "453040"
  ;; :underline-paren-match :unspecified

  ;; Mappings

  ;; General mappings
  :fringe :bg-dim
  :cursor :fg-main

  :keybind :blue-cooler
  :name :magenta
  :identifier :yellow-faint

  :err :red
  :warning :yellow-warmer
  :info :cyan-cooler

  :underline-err :red-intense
  :underline-warning :yellow
  :underline-note :cyan

  :bg-prominent-err :bg-red-intense
  :fg-prominent-err :fg-main
  :bg-prominent-warning :bg-yellow-intense
  :fg-prominent-warning :fg-main
  :bg-prominent-note :bg-cyan-intense
  :fg-prominent-note :fg-main

  :bg-active-argument :bg-yellow-nuanced
  :fg-active-argument :yellow-cooler
  :bg-active-value :bg-cyan-nuanced
  :fg-active-value :cyan-cooler
  ;; Code mappings
  :bracket :fg-main
  :builtin :magenta-warmer
  :comment :fg-dim
  :constant :blue-cooler
  :delimiter :fg-main
  :docmarkup :magenta-faint
  :docstring :cyan-faint
  :fnname :magenta
  :keyword :magenta-cooler
  :number :fg-main
  :operator :fg-main
  :preprocessor :red-cooler
  :property :cyan
  :punctuation :fg-main
  :rx-backslash :magenta
  :rx-construct :green-cooler
  :string :blue-warmer
  :type :cyan-cooler
  :variable :cyan
  ;; Accent mappings
  :accent-0 :blue-cooler
  :accent-1 :magenta-warmer
  :accent-2 :cyan-cooler
  :accent-3 :yellow
  ;; Button mappings
  :fg-button-active :fg-main
  :fg-button-inactive :fg-dim
  :bg-button-active :bg-active
  :bg-button-inactive :bg-dim
  ;; Completion mappings
  :fg-completion-match-0 :blue-cooler
  :fg-completion-match-1 :magenta-warmer
  :fg-completion-match-2 :cyan-cooler
  :fg-completion-match-3 :yellow
  ;; :bg-completion-match-0 :unspecified
  ;; :bg-completion-match-1 :unspecified
  ;; :bg-completion-match-2 :unspecified
  ;; :bg-completion-match-3 :unspecified
  ;; Date mappings
  :date-common :cyan
  :date-deadline :red-cooler
  :date-deadline-subtle :red-faint
  :date-event :fg-alt
  :date-holiday :magenta-warmer
  :date-holiday-other :blue
  :date-now :fg-main
  :date-range :fg-alt
  :date-scheduled :yellow-cooler
  :date-scheduled-subtle :yellow-faint
  :date-weekday :cyan
  :date-weekend :magenta
  ;; Line number mappings
  :fg-line-number-inactive :fg-dim
  :fg-line-number-active :fg-main
  :bg-line-number-inactive :bg-dim
  :bg-line-number-active :bg-active
  ;; Link mappings
  :fg-link :blue-warmer
  ;; :bg-link :unspecified
  :underline-link :blue-warmer

  :fg-link-symbolic :cyan
  ;; :bg-link-symbolic :unspecified
  :underline-link-symbolic :cyan

  :fg-link-visited :magenta
  ;; :bg-link-visited :unspecified
  :underline-link-visited :magenta
  ;; Mail mappings
  :mail-cite-0 :blue-warmer
  :mail-cite-1 :yellow-cooler
  :mail-cite-2 :cyan-cooler
  :mail-cite-3 :red-cooler
  :mail-part :blue
  :mail-recipient :magenta-cooler
  :mail-subject :magenta-warmer
  :mail-other :magenta-faint
  ;; Mark mappings
  :bg-mark-delete :bg-red-subtle
  :fg-mark-delete :red-cooler
  :bg-mark-select :bg-cyan-subtle
  :fg-mark-select :cyan
  :bg-mark-other :bg-yellow-subtle
  :fg-mark-other :yellow
  ;; Prompt mappings
  :fg-prompt :cyan-cooler
  ;; :bg-prompt :unspecified
  ;; Prose mappings
  :bg-prose-block-delimiter :bg-dim
  :fg-prose-block-delimiter :fg-dim
  :bg-prose-block-contents :bg-dim

  ;; :bg-prose-code :unspecified
  :fg-prose-code :cyan-cooler

  ;; :bg-prose-macro :unspecified
  :fg-prose-macro :magenta-cooler

  ;; :bg-prose-verbatim :unspecified
  :fg-prose-verbatim :magenta-warmer

  :prose-done :green
  :prose-todo :red

  :prose-metadata :fg-dim
  :prose-metadata-value :fg-alt

  :prose-table :fg-alt
  :prose-table-formula :magenta-warmer

  :prose-tag :magenta-faint
  ;; Rainbow mappings
  :rainbow-0 :fg-main
  :rainbow-1 :magenta-intense
  :rainbow-2 :cyan-intense
  :rainbow-3 :red-warmer
  :rainbow-4 :yellow-intense
  :rainbow-5 :magenta-cooler
  :rainbow-6 :green-intense
  :rainbow-7 :blue-warmer
  :rainbow-8 :magenta-warmer
  ;; Search mappings
  :bg-search-current :bg-yellow-intense
  :bg-search-lazy :bg-cyan-intense
  :bg-search-replace :bg-red-intense

  :bg-search-rx-group-0 :bg-blue-intense
  :bg-search-rx-group-1 :bg-green-intense
  :bg-search-rx-group-2 :bg-red-subtle
  :bg-search-rx-group-3 :bg-magenta-subtle
  ;; Space mappings
  ;; :bg-space :unspecified
  :fg-space :border
  :bg-space-err :bg-red-intense
  ;; Terminal mappings
  :bg-term-black           "000000"
  :fg-term-black           "000000"
  :bg-term-black-bright    "595959"
  :fg-term-black-bright    "595959"

  :bg-term-red             :red
  :fg-term-red             :red
  :bg-term-red-bright      :red-warmer
  :fg-term-red-bright      :red-warmer

  :bg-term-green           :green
  :fg-term-green           :green
  :bg-term-green-bright    :green-cooler
  :fg-term-green-bright    :green-cooler

  :bg-term-yellow          :yellow
  :fg-term-yellow          :yellow
  :bg-term-yellow-bright   :yellow-warmer
  :fg-term-yellow-bright   :yellow-warmer
  
  :bg-term-blue            :blue
  :fg-term-blue            :blue
  :bg-term-blue-bright     :blue-warmer
  :fg-term-blue-bright     :blue-warmer

  :bg-term-magenta         :magenta
  :fg-term-magenta         :magenta
  :bg-term-magenta-bright  :magenta-cooler
  :fg-term-magenta-bright  :magenta-cooler

  :bg-term-cyan            :cyan
  :fg-term-cyan            :cyan
  :bg-term-cyan-bright     :cyan-cooler
  :fg-term-cyan-bright     :cyan-cooler

  :bg-term-white           "a6a6a6"
  :fg-term-white           "a6a6a6"
  :bg-term-white-bright    "ffffff"
  :fg-term-white-bright    "ffffff"
  ;; Heading mappings
  :fg-heading-0 :cyan-cooler
  :fg-heading-1 :fg-main
  :fg-heading-2 :yellow-faint
  :fg-heading-3 :blue-faint
  :fg-heading-4 :magenta
  :fg-heading-5 :green-faint
  :fg-heading-6 :red-faint
  :fg-heading-7 :cyan-faint
  :fg-heading-8 :fg-dim

  ;; :bg-heading-0 :unspecified
  ;; :bg-heading-1 :unspecified
  ;; :bg-heading-2 :unspecified
  ;; :bg-heading-3 :unspecified
  ;; :bg-heading-4 :unspecified
  ;; :bg-heading-5 :unspecified
  ;; :bg-heading-6 :unspecified
  ;; :bg-heading-7 :unspecified
  ;; :bg-heading-8 :unspecified

  ;; :overline-heading-0 :unspecified
  ;; :overline-heading-1 :unspecified
  ;; :overline-heading-2 :unspecified
  ;; :overline-heading-3 :unspecified
  ;; :overline-heading-4 :unspecified
  ;; :overline-heading-5 :unspecified
  ;; :overline-heading-6 :unspecified
  ;; :overline-heading-7 :unspecified
  ;; :overline-heading-8 :unspecified)
)
