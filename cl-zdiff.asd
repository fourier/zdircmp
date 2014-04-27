(asdf:defsystem #:cl-zdiff
  :description "cl-zdiff: directory comparison tool"
  :version "0.1"
  :author "Alexey Veretennikov <EMAIL-REDACTED>"
  :licence "GPL"
  :depends-on (#:cl-ncurses #:cl-fad #:swank)
  :components ((:file "util")
               (:file "command-ui")
               (:file "constants"
                      :depends-on ("util"))
               (:file "utils-ui"
                      :depends-on ("util"))
               (:file "message-view" :depends-on ("utils-ui"))
               (:file "head-view" :depends-on ("util" "utils-ui"))
               (:file "model-node"
                      :depends-on ("util"))
               (:file "model-tree"
                      :depends-on ("util" "model-node"))
               (:file "main-view"
                      :depends-on ("util"
                                   "command-ui"
                                   "model-node"
                                   "model-tree"
                                   "constants"
                                   "utils-ui"
                                   "message-view"))
               (:file "main"
                      :depends-on ("constants"
                                   "utils-ui"
                                   "message-view"
                                   "head-view"
                                   "main-view"
                                   "utils-ui"
                                   "model-node"))))
