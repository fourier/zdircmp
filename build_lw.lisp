#-quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                      (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
          (load quicklisp-init)))
;;(ql:quickload "cl-ncurses")
;;(ql:quickload "cl-fad")
;;(ql:quickload "alexandria")
;;(ql:quickload :swank)
;; add Sources/ directory to quicklisp local directories
(push #P"/Users/alexeyv/Sources/" ql:*local-project-directories*)
(ql:register-local-projects)
;;
;;(ql:quickload :swank)
(ql:quickload :zdircmp)
;;(save-image "zdircmp-lw" :console t :clean-down t :environment nil :restart-function 'zdircmp.main:main)
(deliver 'zdircmp.main:main
         "./zdircmp-lw"
         0
         :interface nil
         :multiprocessing t)
