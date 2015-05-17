#-quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
          (load quicklisp-init)))

;; add Sources/ directory to quicklisp local directories
(push #P"/Users/alexeyv/Sources/" ql:*local-project-directories*)
(ql:register-local-projects)

;;(ql:quickload :swank)
(ql:quickload :zdircmp)

(save-lisp-and-die "zdircmp" :executable t :toplevel 'zdircmp.main:main)
