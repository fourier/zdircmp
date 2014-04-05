(asdf:defsystem #:cl-zdiff
  :depends-on (#:cl-ncurses #:cl-fad)
  :components ((:file "util")
               (:file "constants")
               (:file "message-view")
               (:file "help-view")
               (:file "model-node"
                      :depends-on ("util"))
               (:file "main-view"
                      :depends-on ("util"
                                   "model-node"
                                   "constants"
                                   "message-view"))
               (:file "tui"
                      :depends-on ("constants"
                                   "message-view"
                                   "help-view"
                                   "main-view"
                                   "model-node"))))
