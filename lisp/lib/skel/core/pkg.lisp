(defpackage :skel/core/condition
  (:use :cl :std/condition :dat/sxp :obj/ast)
  (:import-from :std/macs :eval-always)
  (:export
   :skel-condition
   :skel-error
   :skel-simple-error
   :skel-syntax-error
   :invalid-skel-ast
   :skel-io-error
   :skel-compile-error))

(defpackage :skel/core/proto
  (:use :cl :std)
  (:export
   ;; types
   :license-designator :script-designator :contact-designator
   ;; generics
   :sk-run :sk-new 
   :sk-tangle :sk-weave
   :sk-call :sk-call*
   :sk-load :sk-save
   :sk-print :sk-read
   :sk-build
   :sk-compile :sk-transform
   :sk-write :sk-writeln
   :sk-write-string :sk-write-file
   :sk-read-file :sk-install-user-config
   :sk-find :sk-find*
   :sk-convert :sk-load-component
   :sk-install
   :sk-test
   :sk-deploy
   :sk-fetch
   :sk-pack
   :sk-unpack
   :sk-bundle
   :sk-unbundle
   :sk-register))

(defpackage :skel/core/header
  (:use :cl :std :skel/core/condition :doc)
  (:export
   :make-file-header 
   :make-shebang-file-header 
   :make-source-file-header 
   :file-header-kind
   :file-header
   :make-source-header-comment 
   :make-shebang-comment))

(defpackage :skel/core/var
  (:use :cl :std :skel/core/proto)
  (:import-from :sb-unix :uid-username :unix-getuid)
  (:import-from :ast :*keep-ast*)
  (:import-from :std/path :merge-homedir-pathnames)
  (:import-from :vc :vc-designator)
  (:export :*user-skelrc* :*system-skelrc*
   :*skel-project* :*default-skelrc*
   :*skel-env* :*skel-project*
   :*skel-registry* :*skel-cache* :*skel-store* :*skel-stash*
   :*skel-registry* :*default-skelfile* :*default-skel-user* :*default-skel-vc-kind*
   :*default-skel-cache* :*skelfile-extension* :*skelfile-boundary* :*skel-path*))

(defpackage :skel/core/obj
  (:use :cl :std :obj
   :skel/core/proto :skel/core/condition :skel/core/var
   :dat/sxp :skel/core/header :vc :log)
  (:import-from :uiop :ensure-absolute-pathname :read-file-forms)
  (:export :sk-license :sk-author :sk-stash :sk-cache :sk-registry :sk-user
   :sk-store :sk-push :sk-pull :sk-include
   :sk-tags :edit-skelrc :sk-target :skel
   :sk-meta :def-sk-class :sk-project :sk-source
   :sk-vc :sk-bind :sk-env :make-sk-rule
   :sk-rule :sk-rule-target :sk-rule-source :sk-rule-recipe
   :sk-make :sk-description :sk-kind :sk-rules
   :sk-version
   :sk-command :sk-scripts :sk-script :sk-config
   :sk-snippets :sk-snippet :sk-abbrevs :sk-abbrev
   :sk-user-config :sk-system-config
   :*skel-user-config* :*skel-system-config*
   :sk-src :sk-component :sk-components :sk-mod
   :sk-parent
   :sk-phases))

(defpackage :skel/core/fs
  (:use :cl :std :skel/core/proto :skel/core/condition :skel/core/var :vc :log)
  (:export))

(defpackage :skel/core/util
  (:use :cl :std :skel/core/obj :skel/core/var :skel/core/proto :dat/sxp :skel/core/condition :obj/ast)
  (:import-from :uiop/pathname :pathname-parent-directory-pathname)
  (:import-from :cli :find-exe)
  (:export
   :init-skelrc :load-skelrc
   :init-skel
   :init-user-skelrc :load-user-skelrc
   :init-system-skelrc :load-system-skelrc
   :init-skelfile
   :load-skelfile
   :find-skelfile
   :find-sk-file
   :sk-config-slot
   :sk-project-slot
   :find-project-root
   :setf-skel-vars
   :list-all-projects
   :sk-search-project))

(defpackage :skel/core/vm
  (:use :cl :std :skel/core/condition :sb-vm)
  (:export :make-stack-slot :make-sk-vm :sks-ref :sks-pop :sks-push
           :skel-vm
           :make-skel-vm
           :skel-vm-p
           :copy-skel-vm
           :skel-vm-ip
           :skel-vm-stack
           :*stack-slot-types*
           :stack-slot-type
           :stack-slot
           :*stack-op-types*
           :stack-op-type
           :*skel-op-types*
           :skel-op-type
           :new-skel-arena
           :with-skel-vm
           :with-skel-scope
           :skel-op
           :make-skel-op
           :skel-op-p
           :copy-skel-op
           :skel-op-scope
           :skel-op-body
           :*skel-stack-size*
           :*skel-arena*
           :*skel-scope*
           :init-skel-op-scope
           :*skel-arena-size*
           :init-skel-scope
           :init-skel-value-scope
           :init-skel-function-scope))

(defpackage :skel/core/print
  (:use :cl :std :skel/core/condition :skel/core/obj :skel/core/proto :skel/core/var)
  (:export))

(defpackage :skel/core/plan
  (:use :cl :std :skel/core/condition :skel/core/obj :skel/core/proto :skel/core/var :plan))
