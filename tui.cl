(require 'cl-ncurses)

(defvar *help-window* nil
  "Top help window")

(shadowing-import 'timeout)
(use-package 'cl-ncurses)

(use-package :ztree.constants)

;; import message function from ztree.view.message for easy use of messages
(import 'ztree.view.message::message)

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

(defun process-resize ()
  (let ((maxcols 0)
        (maxrows 0))
    (getmaxyx *stdscr* maxrows maxcols)
    (ztree.view.message:resize-view 0 (1- maxrows) maxcols 1)
    (ztree.view.main:resize-view 0 0 maxcols (1- maxrows))))


(defun handle-key (key)
  ;; handle F1-F12 in main app
  (cond ((eq key +KEY-F1+) 
         (message "F1"))
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
  ;; create the messages window
  (ztree.view.message:create-view 0 (1- *lines*) *cols* 1)
  ;; create the main window
  (ztree.view.main:create-view 0 0 *cols* (1- *lines*))
  (let ((key nil))
    (loop while (not (eq (setf key (getch)) +KEY-ESC+)) do
         (handle-key key)))
  (ztree.view.message:destroy-view)
  (ztree.view.main:destroy-view))

(quit)
