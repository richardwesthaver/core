;;; base16.lisp --- Default Base16 Color Palettes

;; See https://github.com/chriskempson/base16-schemes-source/blob/main/list.yaml
;; - The only other addition is :ZENBURN

;;; Code:
(in-package :obj/color)

;; author: "Chris Kempson (http://chriskempson.com)"
(make-palette :cupcake
  :base00 "fbf1f2"
  :base01 "f2f1f4"
  :base02 "d8d5dd"
  :base03 "bfb9c6"
  :base04 "a59daf"
  :base05 "8b8198"
  :base06 "72677E"
  :base07 "585062"
  :base08 "D57E85"
  :base09 "EBB790"
  :base0A "DCB16C"
  :base0B "A3B367"
  :base0C "69A9A7"
  :base0D "7297B9"
  :base0E "BB99B4"
  :base0F "BAA58C")

;; author: "Chris Kempson (http://chriskempson.com)"
(make-palette :dark
  :base00 "181818"
  :base01 "282828"
  :base02 "383838"
  :base03 "585858"
  :base04 "b8b8b8"
  :base05 "d8d8d8"
  :base06 "e8e8e8"
  :base07 "f8f8f8"
  :base08 "ab4642"
  :base09 "dc9656"
  :base0A "f7ca88"
  :base0B "a1b56c"
  :base0C "86c1b9"
  :base0D "7cafc2"
  :base0E "ba8baf"
  :base0F "a16946")

;; author: "Chris Kempson (http://chriskempson.com)"
(make-palette :light
  :base00 "f8f8f8"
  :base01 "e8e8e8"
  :base02 "d8d8d8"
  :base03 "b8b8b8"
  :base04 "585858"
  :base05 "383838"
  :base06 "282828"
  :base07 "181818"
  :base08 "ab4642"
  :base09 "dc9656"
  :base0A "f7ca88"
  :base0B "a1b56c"
  :base0C "86c1b9"
  :base0D "7cafc2"
  :base0E "ba8baf"
  :base0F "a16946")

;; author: "Chris Kempson (http://chriskempson.com)"
(make-palette :eighties
  :base00 "2d2d2d"
  :base01 "393939"
  :base02 "515151"
  :base03 "747369"
  :base04 "a09f93"
  :base05 "d3d0c8"
  :base06 "e8e6df"
  :base07 "f2f0ec"
  :base08 "f2777a"
  :base09 "f99157"
  :base0A "ffcc66"
  :base0B "99cc99"
  :base0C "66cccc"
  :base0D "6699cc"
  :base0E "cc99cc"
  :base0F "d27b53")

;; author: "Chris Kempson (http://chriskempson.com)"
(make-palette :mocha
  :base00 "3B3228"
  :base01 "534636"
  :base02 "645240"
  :base03 "7e705a"
  :base04 "b8afad"
  :base05 "d0c8c6"
  :base06 "e9e1dd"
  :base07 "f5eeeb"
  :base08 "cb6077"
  :base09 "d28b71"
  :base0A "f4bc87"
  :base0B "beb55b"
  :base0C "7bbda4"
  :base0D "8ab3b5"
  :base0E "a89bb9"
  :base0F "bb9584")

;; author: "Chris Kempson (http://chriskempson.com)"
(make-palette :ocean
  :base00 "2b303b"
  :base01 "343d46"
  :base02 "4f5b66"
  :base03 "65737e"
  :base04 "a7adba"
  :base05 "c0c5ce"
  :base06 "dfe1e8"
  :base07 "eff1f5"
  :base08 "bf616a"
  :base09 "d08770"
  :base0A "ebcb8b"
  :base0B "a3be8c"
  :base0C "96b5b4"
  :base0D "8fa1b3"
  :base0E "b48ead"
  :base0F "ab7967")

;; :author "elnawe"
;; ref: https://github.com/nawetimebomb/base16-zenburn-scheme
(make-palette :zenburn
  :base00 "383838"
  :base01 "404040"
  :base02 "606060"
  :base03 "6f6f6f"
  :base04 "808080"
  :base05 "dcdccc"
  :base06 "c0c0c0"
  :base07 "ffffff"
  :base08 "dca3a3"
  :base09 "dfaf8f"
  :base0A "e0cf9f"
  :base0B "5f7f5f"
  :base0C "93e0e3"
  :base0D "7cb8bb"
  :base0E "dc8cc3"
  :base0F "000000")
