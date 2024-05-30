(defpackage :skel/core/err
  (:use :cl :std :dat/sxp)
  (:export
   :skel-error
   :skel-syntax-error
   :skel-fmt-error
   :skel-compile-error))

(defpackage :skel/core/types
  (:use :cl :std)
  (:export :vc-designator :license-designator :script-designator))

(defpackage :skel/core/proto
  (:use :cl :std)
  (:export
   :sk-run :sk-new :sk-save
   :sk-tangle :sk-weave
   :sk-call :sk-load
   :sk-print :sk-read
   :sk-compile :sk-transform
   :sk-write :sk-writeln
   :sk-write-string :sk-write-file
   :sk-read-file :sk-install-user-config
   :sk-vc-push :sk-vc-pull
   :sk-find-rule :sk-find-script))
   
(defpackage :skel/core/header
  (:use :cl :std :skel/core/err)
  (:export
   :make-file-header 
   :make-shebang-file-header 
   :make-source-file-header 
   :file-header-kind
   :file-header
   :make-source-header-comment 
   :make-shebang-comment))

(defpackage :skel/core/vars
  (:use :cl :std :skel/core/types)
  (:import-from :sb-unix :uid-username :unix-getuid)
  (:export :*user-skelrc* :*system-skelrc* :*keep-ast*
   :*skel-project* :*default-skelrc*
   :*skel-env* :*skel-project*
   :*skel-registry* :*skel-cache* :*skel-store* :*skel-stash*
   :*skel-registry* :*default-skelfile* :*default-skel-user* :*default-skel-vc-kind*
   :*default-skel-cache* :*skelfile-extension* :*skelfile-boundary*))

(defpackage :skel/core/obj
  (:use :cl :std :obj
   :skel/core/proto :skel/core/err :skel/core/types :skel/core/vars
   :dat/sxp :skel/core/header :vc :log)
  (:import-from :uiop :ensure-absolute-pathname :read-file-forms)
  (:export :sk-license :sk-author :sk-path :sk-stash :sk-cache :sk-registry :sk-user
   :sk-store :sk-push :sk-pull :sk-imports
   :sk-tags :edit-skelrc :sk-target :skel
   :sk-meta :def-sk-class :sk-project :sk-source
   :sk-vc :sk-vars :sk-env :make-sk-rule
   :make-sk-vc-meta :sk-vc-meta :sk-vc-meta-kind :sk-vc-meta-remotes
   :sk-rule :sk-rule-target :sk-rule-source :sk-rule-recipe
   :sk-make :sk-description :sk-kind :sk-rules
   :sk-version :sk-name :sk-docs :sk-document 
   :sk-command :sk-scripts :sk-script :sk-config
   :sk-snippets :sk-snippet :sk-abbrevs :sk-abbrev
   :sk-user-config :sk-system-config
   :*sk-user-config* :*sk-system-config*))

(defpackage :skel/core/util
  (:use :cl :std :skel/core/obj :skel/core/vars :skel/core/proto :dat/sxp :skel/core/err)
  (:import-from :uiop/pathname :pathname-parent-directory-pathname)
  (:import-from :cli :find-exe)
  (:export
   :init-skelrc :load-skelrc
   :init-skel-vars
   :init-user-skelrc :load-user-skelrc
   :init-system-skelrc :load-system-skelrc
   :init-skelfile
   :load-skelfile
   :find-skelfile
   :find-sk-file
   :find-project-root
   :describe-skeleton
   :describe-project))

(defpackage :skel/core/vm
  (:use :cl :std :skel/core/err)
  (:export :make-stack-slot :make-sk-vm :sks-ref :sks-pop :sks-push))
