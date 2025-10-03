;;; css.lisp --- Cascading Style Sheets

;; https://www.w3.org/Style/CSS/

;;; Commentary:

;; for a list of all properties refer to:
;; https://www.w3.org/Style/CSS/all-properties.en.html

;; for other web data: https://github.com/mdn/data/tree/main

;; ref: https://github.com/inaimathi/cl-css

;;; Code:
(in-package :dat/css)

;; SHEET     ::= (BLOCK*)
;; BLOCK     ::= (:BLOCK SELECTOR PROPERTY*)
;; SELECTOR  ::= (string*)
;; PROPERTY  ::= (:PROPERTY string string)

;;; Vars
(defvar *minify-css* nil
  "When non-nil, CSS output is minified.")
(defvar *css-indent* nil
  "When non-nil, indicates the number of spaces to use for indentation.")

;; The following variables are derived from Emacs css-mode:
;; https://github.com/emacs-mirror/emacs/blob/master/lisp/textmodes/css-mode.el
(defvar *css-pseudo-class-ids*
  '("active" "checked" "default" "disabled" "empty" "enabled" "first"
    "first-child" "first-of-type" "focus" "focus-within" "hover"
    "in-range" "indeterminate" "invalid" "lang" "last-child"
    "last-of-type" "left" "link" "not" "nth-child" "nth-last-child"
    "nth-last-of-type" "nth-of-type" "only-child" "only-of-type"
    "optional" "out-of-range" "read-only" "read-write" "required"
    "right" "root" "scope" "target" "valid" "visited")
  "Identifiers for pseudo-classes.")

(defvar *css-pseudo-element-ids*
  '("after" "before" "first-letter" "first-line" "selection")
  "Identifiers for pseudo-elements.")

(defvar *css-at-ids*
  '("charset" "font-face" "import" "keyframes" "media" "namespace"
    "page" "supports")
  "Identifiers that appear in the form @foo.")

(defvar *scss-at-ids*
  '("at-root" "content" "debug" "each" "else" "else if" "error" "extend"
    "for" "function" "if" "import" "include" "mixin" "return" "use" "warn"
    "while")
  "Additional identifiers that appear in the form @foo in SCSS.")

(defvar *css-bang-ids*
  '("important")
  "Identifiers that appear in the form !foo.")

(defvar *scss-bang-ids*
  '("default" "global" "optional")
  "Additional identifiers that appear in the form !foo in SCSS.")

(defvar *css-descriptor-ids*
  '("ascent" "baseline" "bbox" "cap-height" "centerline" "definition-src"
    "descent" "font-family" "font-size" "font-stretch" "font-style"
    "font-variant" "font-weight" "mathline" "panose-1" "slope" "src" "stemh"
    "stemv" "topline" "unicode-range" "units-per-em" "widths" "x-height")
  "Identifiers for font descriptors.")

(defvar *css-media-ids*
  '("all" "aural" "bitmap" "continuous" "grid" "paged" "static" "tactile"
    "visual")
  "Identifiers for types of media.")

;; CSS 2.1 properties (https://www.w3.org/TR/CSS21/propidx.html).
(defvar *css-property-alist*
  ;; Properties duplicated by any of the CSS3 modules below have been
  ;; removed.
  '(("azimuth" angle "left-side" "far-left" "left" "center-left"
     "center" "center-right" "right" "far-right" "right-side" "behind"
     "leftwards" "rightwards")
    ("border-collapse" "collapse" "separate")
    ("border-spacing" length)
    ("bottom" length percentage "auto")
    ("caption-side" "top" "bottom")
    ("clear" "none" "left" "right" "both")
    ("content" "normal" "none" string uri counter "attr()"
     "open-quote" "close-quote" "no-open-quote" "no-close-quote")
    ("counter-increment" identifier integer "none")
    ("counter-reset" identifier integer "none")
    ("cue" cue-before cue-after)
    ("cue-after" uri "none")
    ("cue-before" uri "none")
    ("display" "inline" "block" "list-item" "inline-block" "table"
     "inline-table" "table-row-group" "table-header-group"
     "table-footer-group" "table-row" "table-column-group"
     "table-column" "table-cell" "table-caption" "none"
     ;; CSS Flexible Box Layout Module Level 1
     ;; (https://www.w3.org/TR/css3-flexbox/#valdef-display-flex)
     "flex" "inline-flex"
     ;; CSS Grid Layout Module Level 1
     ;; (https://www.w3.org/TR/css-grid-1/#grid-containers)
     "grid" "inline-grid" "subgrid")
    ("elevation" angle "below" "level" "above" "higher" "lower")
    ("empty-cells" "show" "hide")
    ("float" "left" "right" "none")
    ("height" length percentage "auto")
    ("left" length percentage "auto")
    ("line-height" "normal" number length percentage)
    ("list-style" list-style-type list-style-position
     list-style-image)
    ("list-style-image" uri "none")
    ("list-style-position" "inside" "outside")
    ("list-style-type" "disc" "circle" "square" "decimal"
     "decimal-leading-zero" "lower-roman" "upper-roman" "lower-greek"
     "lower-latin" "upper-latin" "armenian" "georgian" "lower-alpha"
     "upper-alpha" "none")
    ("margin" margin-width)
    ("margin-bottom" margin-width)
    ("margin-left" margin-width)
    ("margin-right" margin-width)
    ("margin-top" margin-width)
    ("max-height" length percentage "none")
    ("max-width" length percentage "none")
    ("min-height" length percentage)
    ("min-width" length percentage)
    ("padding" padding-width)
    ("padding-bottom" padding-width)
    ("padding-left" padding-width)
    ("padding-right" padding-width)
    ("padding-top" padding-width)
    ("page-break-after" "auto" "always" "avoid" "left" "right")
    ("page-break-before" "auto" "always" "avoid" "left" "right")
    ("page-break-inside" "avoid" "auto")
    ("pause" time percentage)
    ("pause-after" time percentage)
    ("pause-before" time percentage)
    ("pitch" frequency "x-low" "low" "medium" "high" "x-high")
    ("pitch-range" number)
    ("play-during" uri "mix" "repeat" "auto" "none")
    ("position" "static" "relative" "absolute" "fixed")
    ("quotes" string "none")
    ("richness" number)
    ("right" length percentage "auto")
    ("speak" "normal" "none" "spell-out")
    ("speak-header" "once" "always")
    ("speak-numeral" "digits" "continuous")
    ("speak-punctuation" "code" "none")
    ("speech-rate" number "x-slow" "slow" "medium" "fast" "x-fast"
     "faster" "slower")
    ("stress" number)
    ("table-layout" "auto" "fixed")
    ("top" length percentage "auto")
    ("vertical-align" "baseline" "sub" "super" "top" "text-top"
     "middle" "bottom" "text-bottom" percentage length)
    ("visibility" "visible" "hidden" "collapse")
    ("voice-family" specific-voice generic-voice specific-voice
     generic-voice)
    ("volume" number percentage "silent" "x-soft" "soft" "medium"
     "loud" "x-loud")
    ("width" length percentage "auto")
    ("z-index" "auto" integer)
    ;; CSS Animations
    ;; (https://www.w3.org/TR/css3-animations/#property-index)
    ("animation" single-animation-name time single-timing-function
     single-animation-iteration-count single-animation-direction
     single-animation-fill-mode single-animation-play-state)
    ("animation-delay" time)
    ("animation-direction" single-animation-direction)
    ("animation-duration" time)
    ("animation-fill-mode" single-animation-fill-mode)
    ("animation-iteration-count" single-animation-iteration-count)
    ("animation-name" single-animation-name)
    ("animation-play-state" single-animation-play-state)
    ("animation-timing-function" single-timing-function)
    ;; CSS Backgrounds and Borders Module Level 3
    ;; (https://www.w3.org/TR/css3-background/#property-index)
    ("background" bg-layer final-bg-layer)
    ("background-attachment" attachment)
    ("background-clip" box)
    ("background-color" color)
    ("background-image" bg-image)
    ("background-origin" box)
    ("background-position" position)
    ("background-repeat" repeat-style)
    ("background-size" bg-size)
    ("border" line-width line-style color)
    ("border-bottom" line-width line-style color)
    ("border-bottom-color" color)
    ("border-bottom-left-radius" length percentage)
    ("border-bottom-right-radius" length percentage)
    ("border-bottom-style" line-style)
    ("border-bottom-width" line-width)
    ("border-color" color)
    ("border-image" border-image-source border-image-slice
     border-image-width border-image-outset border-image-repeat)
    ("border-image-outset" length number)
    ("border-image-repeat" "stretch" "repeat" "round" "space")
    ("border-image-slice" number percentage "fill")
    ("border-image-source" "none" image)
    ("border-image-width" length percentage number "auto")
    ("border-left" line-width line-style color)
    ("border-left-color" color)
    ("border-left-style" line-style)
    ("border-left-width" line-width)
    ("border-radius" length percentage)
    ("border-right" line-width line-style color)
    ("border-right-color" color)
    ("border-right-style" line-style)
    ("border-right-width" line-width)
    ("border-style" line-style)
    ("border-top" line-width line-style color)
    ("border-top-color" color)
    ("border-top-left-radius" length percentage)
    ("border-top-right-radius" length percentage)
    ("border-top-style" line-style)
    ("border-top-width" line-width)
    ("border-width" line-width)
    ("box-shadow" "none" shadow)
    ;; CSS Basic User Interface Module Level 3 (CSS3 UI)
    ;; (https://www.w3.org/TR/css3-ui/#property-index)
    ("box-sizing" "content-box" "border-box")
    ("caret-color" "auto" color)
    ("cursor" uri x y "auto" "default" "none" "context-menu" "help"
     "pointer" "progress" "wait" "cell" "crosshair" "text"
     "vertical-text" "alias" "copy" "move" "no-drop" "not-allowed"
     "grab" "grabbing" "e-resize" "n-resize" "ne-resize" "nw-resize"
     "s-resize" "se-resize" "sw-resize" "w-resize" "ew-resize"
     "ns-resize" "nesw-resize" "nwse-resize" "col-resize" "row-resize"
     "all-scroll" "zoom-in" "zoom-out")
    ("nav-down" "auto" id "current" "root" target-name)
    ("nav-left" "auto" id "current" "root" target-name)
    ("nav-right" "auto" id "current" "root" target-name)
    ("nav-up" "auto" id "current" "root" target-name)
    ("outline" outline-color outline-style outline-width)
    ("outline-color" color "invert")
    ("outline-offset" length)
    ("outline-style" "auto" border-style)
    ("outline-width" border-width)
    ("resize" "none" "both" "horizontal" "vertical")
    ("text-overflow" "clip" "ellipsis" string)
    ;; CSS Cascading and Inheritance Level 3
    ;; (https://www.w3.org/TR/css-cascade-3/#property-index)
    ("all")
    ;; CSS Color Module Level 3
    ;; (https://www.w3.org/TR/css3-color/#property)
    ("color" color)
    ("opacity" alphavalue)
    ;; CSS Containment Module Level 2
    ;; (https://www.w3.org/TR/css-contain-2/#property-index)
    ("contain" "none" "strict" "content" "size" "layout" "style" "paint")
    ("content-visibility" "visible" "auto" "hidden")
    ;; CSS Grid Layout Module Level 2
    ;; (https://www.w3.org/TR/css-grid-2/#property-index)
    ("grid" grid-template grid-template-rows "auto-flow" "dense"
     grid-auto-columns grid-auto-rows grid-template-columns)
    ("grid-area" grid-line)
    ("grid-auto-columns" track-size)
    ("grid-auto-flow" "row" "column" "dense")
    ("grid-auto-rows" track-size)
    ("grid-column" grid-line)
    ("grid-column-end" grid-line)
    ("grid-column-gap" length-percentage)
    ("grid-column-start" grid-line)
    ("grid-gap" grid-row-gap grid-column-gap)
    ("grid-row" grid-line)
    ("grid-row-end" grid-line)
    ("grid-row-gap" length-percentage)
    ("grid-row-start" grid-line)
    ("grid-template" "none" grid-template-rows grid-template-columns
     line-names string track-size line-names explicit-track-list)
    ("grid-template-areas" "none" string)
    ("grid-template-columns" "none" track-list auto-track-list "subgrid")
    ("grid-template-rows" "none" track-list auto-track-list "subgrid")
    ;; CSS Box Alignment Module Level 3
    ;; (https://www.w3.org/TR/css-align-3/#property-index)
    ("align-content" baseline-position content-distribution
     overflow-position content-position)
    ("align-items" "normal" "stretch" baseline-position
     overflow-position self-position)
    ("align-self" "auto" "normal" "stretch" baseline-position
     overflow-position self-position)
    ("column-gap" "normal" length-percentage)
    ("gap" row-gap column-gap)
    ("justify-content" "normal" content-distribution overflow-position
     content-position "left" "right")
    ("justify-items" "normal" "stretch" baseline-position
     overflow-position self-position "left" "right" "legacy" "center")
    ("justify-self" "auto" "normal" "stretch" baseline-position
     overflow-position self-position "left" "right")
    ("place-content" align-content justify-content)
    ("place-items" align-items justify-items)
    ("place-self" justify-self align-self)
    ("row-gap" "normal" length-percentage)
    ;; CSS Flexible Box Layout Module Level 1
    ;; (https://www.w3.org/TR/css-flexbox-1/#property-index)
    ("flex" "none" flex-grow flex-shrink flex-basis)
    ("flex-basis" "auto" "content" width)
    ("flex-direction" "row" "row-reverse" "column" "column-reverse")
    ("flex-flow" flex-direction flex-wrap)
    ("flex-grow" number)
    ("flex-shrink" number)
    ("flex-wrap" "nowrap" "wrap" "wrap-reverse")
    ("order" integer)
    ;; CSS Fonts Module Level 3
    ;; (https://www.w3.org/TR/css3-fonts/#property-index)
    ("font" font-style font-variant-css21 font-weight font-stretch
     font-size line-height font-family "caption" "icon" "menu"
     "message-box" "small-caption" "status-bar")
    ("font-family" family-name generic-family)
    ("font-feature-settings" "normal" feature-tag-value)
    ("font-kerning" "auto" "normal" "none")
    ("font-language-override" "normal" string)
    ("font-size" absolute-size relative-size length percentage)
    ("font-size-adjust" "none" number)
    ("font-stretch" "normal" "ultra-condensed" "extra-condensed"
     "condensed" "semi-condensed" "semi-expanded" "expanded"
     "extra-expanded" "ultra-expanded")
    ("font-style" "normal" "italic" "oblique")
    ("font-synthesis" "none" "weight" "style")
    ("font-variant" "normal" "none" common-lig-values
     discretionary-lig-values historical-lig-values
     contextual-alt-values "stylistic()" "historical-forms"
     "styleset()" "character-variant()" "swash()" "ornaments()"
     "annotation()" "small-caps" "all-small-caps" "petite-caps"
     "all-petite-caps" "unicase" "titling-caps" numeric-figure-values
     numeric-spacing-values numeric-fraction-values "ordinal"
     "slashed-zero" east-asian-variant-values east-asian-width-values
     "ruby")
    ("font-variant-alternates" "normal" "stylistic()"
     "historical-forms" "styleset()" "character-variant()" "swash()"
     "ornaments()" "annotation()")
    ("font-variant-caps" "normal" "small-caps" "all-small-caps"
     "petite-caps" "all-petite-caps" "unicase" "titling-caps")
    ("font-variant-east-asian" "normal" east-asian-variant-values
     east-asian-width-values "ruby")
    ("font-variant-ligatures" "normal" "none" common-lig-values
     discretionary-lig-values historical-lig-values
     contextual-alt-values)
    ("font-variant-numeric" "normal" numeric-figure-values
     numeric-spacing-values numeric-fraction-values "ordinal"
     "slashed-zero")
    ("font-variant-position" "normal" "sub" "super")
    ("font-weight" "normal" "bold" "bolder" "lighter" "100" "200"
     "300" "400" "500" "600" "700" "800" "900")
    ;; CSS Fragmentation Module Level 3
    ;; (https://www.w3.org/TR/css-break-3/#property-index)
    ("box-decoration-break" "slice" "clone")
    ("break-after" "auto" "avoid" "avoid-page" "page" "left" "right"
     "recto" "verso" "avoid-column" "column" "avoid-region" "region")
    ("break-before" "auto" "avoid" "avoid-page" "page" "left" "right"
     "recto" "verso" "avoid-column" "column" "avoid-region" "region")
    ("break-inside" "auto" "avoid" "avoid-page" "avoid-column"
     "avoid-region")
    ("orphans" integer)
    ("widows" integer)
    ;; CSS Masking Module Level 1
    ;; (https://www.w3.org/TR/css-masking-1/#property-index)
    ("clip-path" clip-source basic-shape geometry-box "none")
    ("clip-rule" "nonzero" "evenodd")
    ("mask-image" mask-reference)
    ("mask-mode" masking-mode)
    ("mask-repeat" repeat-style)
    ("mask-position" position)
    ("mask-clip" geometry-box "no-clip")
    ("mask-origin" geometry-box)
    ("mask-size" bg-size)
    ("mask-composite" compositing-operator)
    ("mask" mask-layer)
    ("mask-border-source" "none" image)
    ("mask-border-mode" "luminance" "alpha")
    ("mask-border-slice" number percentage "fill")
    ("mask-border-width" length percentage number "auto")
    ("mask-border-outset" length number)
    ("mask-border-repeat" "stretch" "repeat" "round" "space")
    ("mask-border" mask-border-source mask-border-slice
     mask-border-width mask-border-outset mask-border-repeat
     mask-border-mode)
    ("mask-type" "luminance" "alpha")
    ("clip" "rect()" "auto")
    ;; CSS Multi-column Layout Module Level 1
    ;; (https://www.w3.org/TR/css3-multicol/#property-index)
    ;; "break-after", "break-before", and "break-inside" are left out
    ;; below, because they're already included in CSS Fragmentation
    ;; Module Level 3.
    ("column-count" "auto" integer)
    ("column-fill" "auto" "balance" "balance-all")
    ("column-rule" column-rule-width column-rule-style
     column-rule-color)
    ("column-rule-color" color)
    ("column-rule-style" line-style)
    ("column-rule-width" line-width)
    ("column-span" "none" "all")
    ("column-width" "auto" length)
    ("columns" column-width column-count)
    ;; CSS Overflow Module Level 3
    ;; (https://www.w3.org/TR/css-overflow-3/#property-index)
    ("max-lines" "none" integer)
    ("overflow" "visible" "hidden" "scroll" "auto" "paged-x" "paged-y"
     "paged-x-controls" "paged-y-controls" "fragments")
    ("overflow-x" "visible" "hidden" "scroll" "auto" "paged-x"
     "paged-y" "paged-x-controls" "paged-y-controls" "fragments")
    ("overflow-y" "visible" "hidden" "scroll" "auto" "paged-x"
     "paged-y" "paged-x-controls" "paged-y-controls" "fragments")
    ;; CSS Text Decoration Module Level 3
    ;; (https://dev.w3.org/csswg/css-text-decor-3/#property-index)
    ("text-decoration" text-decoration-line text-decoration-style
     text-decoration-color)
    ("text-decoration-color" color)
    ("text-decoration-line" "none" "underline" "overline"
     "line-through" "blink")
    ("text-decoration-skip" "none" "objects" "spaces" "ink" "edges"
     "box-decoration")
    ("text-decoration-style" "solid" "double" "dotted" "dashed"
     "wavy")
    ("text-emphasis" text-emphasis-style text-emphasis-color)
    ("text-emphasis-color" color)
    ("text-emphasis-position" "over" "under" "right" "left")
    ("text-emphasis-style" "none" "filled" "open" "dot" "circle"
     "double-circle" "triangle" "sesame" string)
    ("text-shadow" "none" length color)
    ("text-underline-position" "auto" "under" "left" "right")
    ;; CSS Text Module Level 3
    ;; (https://www.w3.org/TR/css3-text/#property-index)
    ("hanging-punctuation" "none" "first" "force-end" "allow-end"
     "last")
    ("hyphens" "none" "manual" "auto")
    ("letter-spacing" "normal" length)
    ("line-break" "auto" "loose" "normal" "strict")
    ("overflow-wrap" "normal" "break-word")
    ("tab-size" integer length)
    ("text-align" "start" "end" "left" "right" "center" "justify"
     "match-parent")
    ("text-align-last" "auto" "start" "end" "left" "right" "center"
     "justify")
    ("text-indent" length percentage)
    ("text-justify" "auto" "none" "inter-word" "distribute")
    ("text-transform" "none" "capitalize" "uppercase" "lowercase"
     "full-width")
    ("white-space" "normal" "pre" "nowrap" "pre-wrap" "pre-line")
    ("word-break" "normal" "keep-all" "break-all")
    ("word-spacing" "normal" length percentage)
    ("word-wrap" "normal" "break-word")
    ;; CSS Transforms Module Level 1
    ;; (https://www.w3.org/TR/css3-2d-transforms/#property-index)
    ("backface-visibility" "visible" "hidden")
    ("perspective" "none" length)
    ("perspective-origin" "left" "center" "right" "top" "bottom"
     percentage length)
    ("transform" "none" transform-list)
    ("transform-origin" "left" "center" "right" "top" "bottom"
     percentage length)
    ("transform-style" "flat" "preserve-3d")
    ;; CSS Transitions
    ;; (https://www.w3.org/TR/css3-transitions/#property-index)
    ("transition" single-transition)
    ("transition-delay" time)
    ("transition-duration" time)
    ("transition-property" "none" single-transition-property "all")
    ("transition-timing-function" single-transition-timing-function)
    ;; CSS Will Change Module Level 1
    ;; (https://www.w3.org/TR/css-will-change-1/#property-index)
    ("will-change" "auto" animateable-feature)
    ;; CSS Writing Modes Level 3
    ;; (https://www.w3.org/TR/css-writing-modes-3/#property-index)
    ;; "glyph-orientation-vertical" is obsolete and left out.
    ("direction" "ltr" "rtl")
    ("text-combine-upright" "none" "all")
    ("text-orientation" "mixed" "upright" "sideways")
    ("unicode-bidi" "normal" "embed" "isolate" "bidi-override"
     "isolate-override" "plaintext")
    ("writing-mode" "horizontal-tb" "vertical-rl" "vertical-lr")
    ;; Filter Effects Module Level 1
    ;; (https://www.w3.org/TR/filter-effects/#property-index)
    ("color-interpolation-filters" "auto" "sRGB" "linearRGB")
    ("filter" "none" filter-function-list)
    ("flood-color" color)
    ("flood-opacity" number percentage)
    ("lighting-color" color)
    ;; Pointer Events
    ;; (https://www.w3.org/TR/pointerevents/#the-touch-action-css-property)
    ("touch-action" "auto" "none" "pan-x" "pan-y" "manipulation"))
  "Identifiers for properties and their possible values.
The CAR of each entry is the name of a property, while the CDR is a list of
possible values for that property.  String values in the CDRs represent
literal values, while symbols represent one of the value classes found in
css-value-class-alist.  If a symbol is not found in css-value-class-alist,
it's interpreted as a reference back to one of the properties in this list.
Some symbols, such as 'number or 'identifier don't produce any further value
candidates, since that list would be infinite.")

(defvar *css-property-ids*
  (mapcar #'car *css-property-alist*)
  "Identifiers for properties.")

(defvar *css--color-map*
  '(("black" . "#000000")
    ("silver" . "#c0c0c0")
    ("gray" . "#808080")
    ("white" . "#ffffff")
    ("maroon" . "#800000")
    ("red" . "#ff0000")
    ("purple" . "#800080")
    ("fuchsia" . "#ff00ff")
    ("magenta" . "#ff00ff")
    ("green" . "#008000")
    ("lime" . "#00ff00")
    ("olive" . "#808000")
    ("yellow" . "#ffff00")
    ("navy" . "#000080")
    ("blue" . "#0000ff")
    ("teal" . "#008080")
    ("aqua" . "#00ffff")
    ("cyan" . "#00ffff")
    ("orange" . "#ffa500")
    ("aliceblue" . "#f0f8ff")
    ("antiquewhite" . "#faebd7")
    ("aquamarine" . "#7fffd4")
    ("azure" . "#f0ffff")
    ("beige" . "#f5f5dc")
    ("bisque" . "#ffe4c4")
    ("blanchedalmond" . "#ffebcd")
    ("blueviolet" . "#8a2be2")
    ("brown" . "#a52a2a")
    ("burlywood" . "#deb887")
    ("cadetblue" . "#5f9ea0")
    ("chartreuse" . "#7fff00")
    ("chocolate" . "#d2691e")
    ("coral" . "#ff7f50")
    ("cornflowerblue" . "#6495ed")
    ("cornsilk" . "#fff8dc")
    ("crimson" . "#dc143c")
    ("darkblue" . "#00008b")
    ("darkcyan" . "#008b8b")
    ("darkgoldenrod" . "#b8860b")
    ("darkgray" . "#a9a9a9")
    ("darkgreen" . "#006400")
    ("darkgrey" . "#a9a9a9")
    ("darkkhaki" . "#bdb76b")
    ("darkmagenta" . "#8b008b")
    ("darkolivegreen" . "#556b2f")
    ("darkorange" . "#ff8c00")
    ("darkorchid" . "#9932cc")
    ("darkred" . "#8b0000")
    ("darksalmon" . "#e9967a")
    ("darkseagreen" . "#8fbc8f")
    ("darkslateblue" . "#483d8b")
    ("darkslategray" . "#2f4f4f")
    ("darkslategrey" . "#2f4f4f")
    ("darkturquoise" . "#00ced1")
    ("darkviolet" . "#9400d3")
    ("deeppink" . "#ff1493")
    ("deepskyblue" . "#00bfff")
    ("dimgray" . "#696969")
    ("dimgrey" . "#696969")
    ("dodgerblue" . "#1e90ff")
    ("firebrick" . "#b22222")
    ("floralwhite" . "#fffaf0")
    ("forestgreen" . "#228b22")
    ("gainsboro" . "#dcdcdc")
    ("ghostwhite" . "#f8f8ff")
    ("gold" . "#ffd700")
    ("goldenrod" . "#daa520")
    ("greenyellow" . "#adff2f")
    ("grey" . "#808080")
    ("honeydew" . "#f0fff0")
    ("hotpink" . "#ff69b4")
    ("indianred" . "#cd5c5c")
    ("indigo" . "#4b0082")
    ("ivory" . "#fffff0")
    ("khaki" . "#f0e68c")
    ("lavender" . "#e6e6fa")
    ("lavenderblush" . "#fff0f5")
    ("lawngreen" . "#7cfc00")
    ("lemonchiffon" . "#fffacd")
    ("lightblue" . "#add8e6")
    ("lightcoral" . "#f08080")
    ("lightcyan" . "#e0ffff")
    ("lightgoldenrodyellow" . "#fafad2")
    ("lightgray" . "#d3d3d3")
    ("lightgreen" . "#90ee90")
    ("lightgrey" . "#d3d3d3")
    ("lightpink" . "#ffb6c1")
    ("lightsalmon" . "#ffa07a")
    ("lightseagreen" . "#20b2aa")
    ("lightskyblue" . "#87cefa")
    ("lightslategray" . "#778899")
    ("lightslategrey" . "#778899")
    ("lightsteelblue" . "#b0c4de")
    ("lightyellow" . "#ffffe0")
    ("limegreen" . "#32cd32")
    ("linen" . "#faf0e6")
    ("mediumaquamarine" . "#66cdaa")
    ("mediumblue" . "#0000cd")
    ("mediumorchid" . "#ba55d3")
    ("mediumpurple" . "#9370db")
    ("mediumseagreen" . "#3cb371")
    ("mediumslateblue" . "#7b68ee")
    ("mediumspringgreen" . "#00fa9a")
    ("mediumturquoise" . "#48d1cc")
    ("mediumvioletred" . "#c71585")
    ("midnightblue" . "#191970")
    ("mintcream" . "#f5fffa")
    ("mistyrose" . "#ffe4e1")
    ("moccasin" . "#ffe4b5")
    ("navajowhite" . "#ffdead")
    ("oldlace" . "#fdf5e6")
    ("olivedrab" . "#6b8e23")
    ("orangered" . "#ff4500")
    ("orchid" . "#da70d6")
    ("palegoldenrod" . "#eee8aa")
    ("palegreen" . "#98fb98")
    ("paleturquoise" . "#afeeee")
    ("palevioletred" . "#db7093")
    ("papayawhip" . "#ffefd5")
    ("peachpuff" . "#ffdab9")
    ("peru" . "#cd853f")
    ("pink" . "#ffc0cb")
    ("plum" . "#dda0dd")
    ("powderblue" . "#b0e0e6")
    ("rosybrown" . "#bc8f8f")
    ("royalblue" . "#4169e1")
    ("saddlebrown" . "#8b4513")
    ("salmon" . "#fa8072")
    ("sandybrown" . "#f4a460")
    ("seagreen" . "#2e8b57")
    ("seashell" . "#fff5ee")
    ("sienna" . "#a0522d")
    ("skyblue" . "#87ceeb")
    ("slateblue" . "#6a5acd")
    ("slategray" . "#708090")
    ("slategrey" . "#708090")
    ("snow" . "#fffafa")
    ("springgreen" . "#00ff7f")
    ("steelblue" . "#4682b4")
    ("tan" . "#d2b48c")
    ("thistle" . "#d8bfd8")
    ("tomato" . "#ff6347")
    ("turquoise" . "#40e0d0")
    ("violet" . "#ee82ee")
    ("wheat" . "#f5deb3")
    ("whitesmoke" . "#f5f5f5")
    ("yellowgreen" . "#9acd32")
    ("rebeccapurple" . "#663399"))
  "Map CSS named colors to their hex RGB value.")

(defvar *css-value-class-alist*
  `((absolute-size
     "xx-small" "x-small" "small" "medium" "large" "x-large"
     "xx-large")
    (alphavalue number)
    (angle "calc()")
    (animateable-feature "scroll-position" "contents" custom-ident)
    (attachment "scroll" "fixed" "local")
    (auto-repeat "repeat()")
    (auto-track-list line-names fixed-size fixed-repeat auto-repeat)
    (basic-shape "inset()" "circle()" "ellipse()" "polygon()")
    (bg-image image "none")
    (bg-layer bg-image position repeat-style attachment box)
    (bg-size length percentage "auto" "cover" "contain")
    (box "border-box" "padding-box" "content-box")
    (clip-source uri)
    (color
     "rgb()" "rgba()" "hsl()" "hsla()" named-color "transparent"
     "currentColor")
    (common-lig-values "common-ligatures" "no-common-ligatures")
    (compositing-operator "add" "subtract" "intersect" "exclude")
    (contextual-alt-values "contextual" "no-contextual")
    (counter "counter()" "counters()")
    (discretionary-lig-values
     "discretionary-ligatures" "no-discretionary-ligatures")
    (east-asian-variant-values
     "jis78" "jis83" "jis90" "jis04" "simplified" "traditional")
    (east-asian-width-values "full-width" "proportional-width")
    (explicit-track-list line-names track-size)
    (family-name "Courier" "Helvetica" "Times")
    (feature-tag-value string integer "on" "off")
    (filter-function
     "blur()" "brightness()" "contrast()" "drop-shadow()"
     "grayscale()" "hue-rotate()" "invert()" "opacity()" "sepia()"
     "saturate()")
    (filter-function-list filter-function uri)
    (final-bg-layer
     bg-image position repeat-style attachment box color)
    (fixed-breadth length-percentage)
    (fixed-repeat "repeat()")
    (fixed-size fixed-breadth "minmax()")
    (font-variant-css21 "normal" "small-caps")
    (frequency "calc()")
    (generic-family
     "serif" "sans-serif" "cursive" "fantasy" "monospace")
    (generic-voice "male" "female" "child")
    (geometry-box shape-box "fill-box" "stroke-box" "view-box")
    (gradient
     linear-gradient radial-gradient repeating-linear-gradient
     repeating-radial-gradient)
    (grid-line "auto" custom-ident integer "span")
    (historical-lig-values
     "historical-ligatures" "no-historical-ligatures")
    (image uri image-list element-reference gradient)
    (image-list "image()")
    (inflexible-breadth length-percentage "min-content" "max-content"
                        "auto")
    (integer "calc()")
    (length "calc()" number)
    (line-height "normal" number length percentage)
    (line-names custom-ident)
    (line-style
     "none" "hidden" "dotted" "dashed" "solid" "double" "groove"
     "ridge" "inset" "outset")
    (line-width length "thin" "medium" "thick")
    (linear-gradient "linear-gradient()")
    (margin-width "auto" length percentage)
    (mask-layer
     mask-reference masking-mode position bg-size repeat-style
     geometry-box "no-clip" compositing-operator)
    (mask-reference "none" image mask-source)
    (mask-source uri)
    (masking-mode "alpha" "luminance" "auto")
    (named-color . ,(mapcar #'car *css--color-map*))
    (number "calc()")
    (numeric-figure-values "lining-nums" "oldstyle-nums")
    (numeric-fraction-values "diagonal-fractions" "stacked-fractions")
    (numeric-spacing-values "proportional-nums" "tabular-nums")
    (padding-width length percentage)
    (position
     "left" "center" "right" "top" "bottom" percentage length)
    (baseline-position "left" "right" "baseline")
    (content-distribution
     "space-between" "space-around" "space-evenly" "stretch")
    (overflow-position "unsafe" "safe")
    (content-position "center" "start" "end" "flex-start" "flex-end")
    (self-position
     "center" "start" "end" "self-start" "self-end" "flex-start" "flex-end")
pdp10    (radial-gradient "radial-gradient()")
    (relative-size "larger" "smaller")
    (repeat-style
     "repeat-x" "repeat-y" "repeat" "space" "round" "no-repeat")
    (repeating-linear-gradient "repeating-linear-gradient()")
    (repeating-radial-gradient "repeating-radial-gradient()")
    (shadow "inset" length color)
    (shape-box box "margin-box")
    (single-animation-direction
     "normal" "reverse" "alternate" "alternate-reverse")
    (single-animation-fill-mode "none" "forwards" "backwards" "both")
    (single-animation-iteration-count "infinite" number)
    (single-animation-name "none" identifier)
    (single-animation-play-state "running" "paused")
    (single-timing-function single-transition-timing-function)
    (single-transition
     "none" single-transition-property time
     single-transition-timing-function)
    (single-transition-property "all" identifier)
    (single-transition-timing-function
     "ease" "linear" "ease-in" "ease-out" "ease-in-out" "step-start"
     "step-end" "steps()" "cubic-bezier()")
    (specific-voice identifier)
    (target-name string)
    (time "calc()")
    (track-breadth length-percentage flex "min-content" "max-content"
                   "auto")
    (track-list line-names track-size track-repeat)
    (track-repeat "repeat()")
    (track-size track-breadth "minmax()" "fit-content()")
    (transform-list
     "matrix()" "translate()" "translateX()" "translateY()" "scale()"
     "scaleX()" "scaleY()" "rotate()" "skew()" "skewX()" "skewY()"
     "matrix3d()" "translate3d()" "translateZ()" "scale3d()"
     "scaleZ()" "rotate3d()" "rotateX()" "rotateY()" "rotateZ()"
     "perspective()")
    (uri "url()")
    (width length percentage "auto")
    (x number)
    (y number))
  "Property value classes and their values.
The format is similar to that of *css-property-alist*, except
that the CARs aren't actual CSS properties, but rather a name for
a class of values, and that symbols in the CDRs always refer to
other entries in this list, not to properties.

The following classes have been left out above because they
cannot be completed sensibly: `custom-ident',
`element-reference', `flex', `id', `identifier',
`length-percentage', `percentage', and `string'.")

;;; Utils
(defun %-or-word (v) 
  (etypecase v
    (number (concatenate 'string (write-to-string v) "%"))
    (null nil)
    (symbol (symbol-name v))
    (string v)))

(defmacro split-directive (directive-name value &optional (prefix-list '(-ms- -o- -webkit- -moz-)))
  (with-gensyms (val)
    `(let ((,val ,value)) 
       (list ,directive-name ,val
	     ,@(loop 
		 for p in prefix-list
		 collect (keywordicate p directive-name)
		 collect val)))))

;; unit helpers
(defun px (val) (format nil "~apx" val))
(defun % (val) (format nil "~a%" val))
(defun em (val) (format nil "~aem" val))

;;; transform
(defun transform-origin (x y &optional z)
  "Takes x, y, z percentages, returns a cross-browser CSS3 transform-origin directive"
  (split-directive :transform (apply #'format nil "~a ~a~@[ ~a~]" (mapcar #'%-or-word (list x y z)))))

(defun rotate (degrees)
  "Takes a number of degrees, returns a cross-browser CSS3 rotate directive"
  (split-directive :transform (format nil "rotate(~adeg)" degrees)))

(defun scale (scale-x &optional (scale-y scale-x))
  "Takes an x and y scale factor, returns x-browser CSS3 scale directive"
  (split-directive :transform (format nil "scale(~a,~a)" scale-x scale-y)))

(defun skew (x-deg y-deg)
  (split-directive :transform (format nil "skew(~adeg, ~adeg)" x-deg y-deg)))

(defun translate (x y &key (units :px))
  "Takes an x and y, returns a x-browser CSS3 translate directive.
units should be either :px (the default) or :%."
  (split-directive :transform (format nil "translate(~a~a, ~a~a)" x units y units)))

(defun matrix (&rest 6-numbers)
  "Takes six numbers and uses them to build a CSS3 transformation matrix directive"
  (split-directive :transform (format nil "matrix(~{~a~^,~})" 6-numbers)))

;;; 3d-transform
(defun perspective (n)
  (split-directive :perspective n (-webkit-)))

(defun perspective-origin (x y)
  (split-directive :perspective-origin 
      (concatenate 'string (%-or-word x) " " (%-or-word y)) (-webkit-)))

(defun backface-visibility (visible/hidden)
  (split-directive :backface-visibility visible/hidden (-webkit- -moz-)))

(defun transform-style (flat/preserve-3d)
  (split-directive :transform-style flat/preserve-3d (-webkit-)))

(defun matrix3d (&rest 16-numbers)
  (split-directive :transform (format nil "matrix3d(~{~a~^,~})" 16-numbers) (-webkit- -moz-)))

(defun translate3d (x y z &key (units :px))
  "Takes an x and y, returns a x-browser CSS3 translate directive.
units should be either :px (the default) or :%."
  (split-directive :transform 
      (format nil "translate3d(~a~a, ~a~a, ~a~a)" x units y units z units) (-webkit- -moz-)))

(defun scale3d (scale-x &optional (scale-y scale-x) (scale-z scale-x))
  "Takes an x and y scale factor, returns x-browser CSS3 scale directive"
  (split-directive :transform (format nil "scale3d(~a,~a,~a)" scale-x scale-y scale-z) (-webkit- -moz-)))

(defun rotate3d (degrees)
  "Takes a number of degrees, returns a cross-browser CSS3 rotate directive"
  (split-directive :transform (format nil "rotate3d(~adeg)" degrees) (-webkit- -moz-)))

;;; animations/transitions
(defun keyframes (animation-name &rest keyframes)
  (flet ((sel (browser-type) 
	   (list (format nil "@~@[~(~a~)~]keyframes ~a" browser-type (format-selector animation-name)) 
		 keyframes)))
    `(,(sel nil) ,(sel :-moz-) ,(sel :-webkit-))))

(defun animation (name &key (duration 0) (timing-function :linear) (delay 0) (iteration-count 1) (direction :normal) (play-state :running))
  (split-directive 
      :animation 
      (format nil "~a ~as ~a ~as ~a ~a ~a"
	      name duration timing-function delay iteration-count direction play-state)
      (-webkit- -moz-)))

(defun transition (property &key (duration 0) (timing-function :ease) (delay 0))
  (split-directive
      :transition
      (format nil "~a ~as ~a ~as" property duration timing-function delay)
      (-webkit- -moz- -o-)))

;;; format
(defun format-selector (s)
  (if (stringp s) s (string-downcase s)))

(defun format-declaration (k v)
  (etypecase v
    (null (format-declarations-list k))
    (number (format nil "~(~A~): ~A;" k v))
    (symbol (concatenate 'string (string-downcase k) ": " (string-downcase v) ";"))
    (string (concatenate 'string (string-downcase k) ": " v ";"))
    (vector (format nil "~(~A~): ~{~A~^, ~};"
                    k
                    (mapcar
                     (lambda (x)
                       (typecase x
                         (list (eval x))
                         (function (funcall x))
                         (symbol (string-downcase x))
                         (t (format nil "~A" x))))
                     (coerce v 'list))))
    (list 
     (if (fboundp (car v))
         (format nil "~(~A~): ~A;" 
                 k 
                 (let ((x (apply (car v) (cdr v))))
                   (typecase x
                     (color (print-hex-rgb x))
                     (t x))))
         (concatenate 'string (string-downcase k) " { " (format-declarations-list v) "}")))))

(defun format-declarations-list (list-of-declarations)
  (apply #'concatenate 
	 'string 
	 (loop with remaining = list-of-declarations
	    for head = (pop remaining) 
	    if (consp head) collect (format-rule (car head) (cdr head))
	    else if head collect (format-declaration head (pop remaining))
	    collect " "
	    while remaining)))

(defun format-rule (selector declarations)
  (concatenate 'string (format-selector selector) 
	       " { " (format-declarations-list declarations) "}"))

;;; generator
(defun inline-css (rule) (format-declarations-list rule))

(defun css (rules)
  (apply #'concatenate 
	 'string 
	 (loop for r in rules
	    collect (format-rule (car r) (cdr r))
	    collect (list #\Newline))))

(defun compile-css (file-path directives)
  (ensure-directories-exist file-path)
  (with-open-file (stream file-path :direction :output :if-exists :supersede :if-does-not-exist :create) 
    (format stream (css directives))))

(definline compile-css-file (input output)
  (dat/css:compile-css output (read-lisp-file input)))
