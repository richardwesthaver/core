(in-package :vc)

(deferror vc-error (std-error) ())

(deferror git-error (vc-error) ())

(deferror hg-error (vc-error) ())
