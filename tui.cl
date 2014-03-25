(require 'cl-ncurses)

(shadowing-import 'timeout)
(use-package 'cl-ncurses)

(use-package :ztree.constants)

;; import message function from ztree.view.message for easy use of messages
(import 'ztree.view.message::message)

(defconstant +min-screen-width+ 60
  "Minimum supported screen width")

(defconstant +min-screen-height+ 15
  "Minimum supported screen height")

(defconstant +help-window-height+ 5
  "Height of the help window")

(defvar *help-window-visible* t)

(defmacro with-ncurses (&body body)
  "Macro to create a ncurses environment"
  `(unwind-protect
        (progn
          (initscr)
          (refresh)
          ,@body)
     (progn
       (endwin))))

;; (macroexpand-1 '(with-ncurses
;;                  (keypad *stdscr* 1)
;;                  ;; turn on color 
;;                  (start-color)))


(defmacro with-ncurses-win (win lines cols y x &body body )
  "Macro to create and operate with the ncurses-window with border.
Params: `win' is a window name,
        `lines' number of lines of the window size,
        `cols' number of columns of the window size,
        `y',`x' - upper left-hand corner of the window
        `body' a list of expressions to perform on `win'"
  `(let ((,win (newwin ,lines ,cols ,y ,x)))
     (unwind-protect
          (progn
            (box ,win 0 0)
            (wrefresh ,win)
            ,@body)
       (progn
         (wborder ,win 32 32 32 32 32 32 32 32)
         (wrefresh ,win)
         (delwin ,win)))))

;; (macroexpand-1 '(with-ncurses-win main 20 20 1 2
;;                  (wprintw main "test")))

(defmacro with-ncurses-borderless-win (win lines cols y x &body body )
  "Macro to create and operate with the ncurses-window without border.
Params: `win' is a window name,
        `lines' number of lines of the window size,
        `cols' number of columns of the window size,
        `y',`x' - upper left-hand corner of the window
        `body' a list of expressions to perform on `win'"
  `(let ((,win (newwin ,lines ,cols ,y ,x)))
     (unwind-protect
          (progn
            ,@body)
       (progn
         (delwin ,win)))))

(define-condition on-bad-screen-size (error)
  ((message :initarg :description :reader description)))


(defun assert-screen-sizes-ok (width height)
  (unless (and (>= width +min-screen-width+)
               (>= height +min-screen-height+))
    (signal 'on-bad-screen-size :description
            (format nil
                    "Too small window size: ~ax~a, required size ~ax~a"
                    width height +min-screen-width+ +min-screen-height+))))


(defun process-resize ()
  (let ((maxcols 0)
        (maxrows 0))
    (getmaxyx *stdscr* maxrows maxcols)
    (assert-screen-sizes-ok maxcols maxrows)
    (ztree.view.message:resize-view 0 (1- maxrows) maxcols 1)
    (let ((main-view-height (1- maxrows))
          (main-view-y 0))
      (when *help-window-visible*
        (ztree.view.help:resize-view 0 0 maxcols +help-window-height+)
        (setf main-view-height (- main-view-height +help-window-height+))
        (setf main-view-y (+ main-view-y +help-window-height+)))
      ;; create the main window
      (ztree.view.main:resize-view 0 main-view-y maxcols main-view-height))))


(defun toggle-help-view ()
  (setf *help-window-visible* (not *help-window-visible*))
  (if *help-window-visible*
      (let ((maxcols 0)
            (maxrows 0))
        (getmaxyx *stdscr* maxrows maxcols)
        (ztree.view.help:create-view 0 0 maxcols +help-window-height+))
    (ztree.view.help:destroy-view))
  (process-resize)
  (message (format nil "~a help window"
                   (if *help-window-visible* "Showing" "Hiding"))))


(defun handle-key (key)
  ;; handle F1-F12 in main app
  (cond ((eq key +KEY-F1+)
         (toggle-help-view))
        ((eq key +KEY-F2+) 
         (message "F2"))
        ((eq key +KEY-F3+) 
         (message "F3"))
        ((eq key +KEY-F4+) 
         (message "F4"))
        ((eq key +KEY-F5+) 
         (message "F5"))
        ((eq key +KEY-F6+) 
         (message "F6"))
        ((eq key +KEY-F7+) 
         (message "F7"))
        ((eq key +KEY-F8+) 
         (message "F8"))
        ((eq key +KEY-F9+) 
         (message "F9"))
        ((eq key +KEY-F10+) 
         (message "F10"))
        ((eq key +KEY-F11+) 
         (message "F11"))
        ((eq key +KEY-F12+) 
         (message "F12"))
        ;; handle resize event
        ((eq key -1)
         (process-resize))
        ;; process others in main view
        (t (ztree.view.main:process-key key))))



(with-ncurses
  ;; no caching of the input
  (cbreak)
  ;; turn on special keys 
  (keypad *stdscr* 1)
  ;; turn on color 
  (start-color)
  ;; turn off key echoing
  (noecho)
  ;; hide cursor
  (curs-set 0)

  ;; get the screen dimensions
  (let ((maxcols 0)
        (maxrows 0))
    (getmaxyx *stdscr* maxrows maxcols)
    (handler-case
        (progn
          ;; verify the screen size
          (assert-screen-sizes-ok maxcols maxrows)
          ;; create the messages window
          (ztree.view.message:create-view 0 (1- *lines*) *cols* 1)
          ;; create a help window if necessary
          (let ((main-view-height (1- *lines*))
                (main-view-y 0))
            (when *help-window-visible*
              (ztree.view.help:create-view 0 0 *cols* +help-window-height+)
              (setf main-view-height (- main-view-height +help-window-height+))
              (setf main-view-y (+ main-view-y +help-window-height+)))
          ;; create the main window
            (ztree.view.main:create-view 0 main-view-y *cols* main-view-height))
          ;; keyboard input loop with ESC as an exit condition
          (let ((key nil))
            (loop while (not (eq (setf key (getch)) +KEY-ESC+)) do
                 (handle-key key))))
      ;; error handling: wrong screen size
      (on-bad-screen-size (what) (format *error-output* (description what)))))
  ;; destroy windows
  (ztree.view.help:destroy-view)
  (ztree.view.message:destroy-view)
  (ztree.view.main:destroy-view))

(quit)
