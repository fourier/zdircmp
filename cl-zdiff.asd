(asdf:defsystem #:cl-zdiff
  :description "cl-zdiff: directory comparison tool"
  :version "0.1"
  :author "Alexey Veretennikov <EMAIL-REDACTED>"
  :licence "GPL"
  :depends-on (#:cl-ncurses #:cl-fad)
  :components ((:file "util")
               (:file "constants")
               (:file "message-view")
               (:file "help-view")
               (:file "model-node"
                      :depends-on ("util"))
               (:file "model-tree"
                      :depends-on ("util" "model-node"))
               (:file "main-view"
                      :depends-on ("util"
                                   "model-node"
                                   "model-tree"
                                   "constants"
                                   "message-view"))
               (:file "tui"
                      :depends-on ("constants"
                                   "message-view"
                                   "help-view"
                                   "main-view"
                                   "model-node"))))
