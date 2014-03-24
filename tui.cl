(require 'cl-ncurses)

;; base-8 constants for getch, from curses.h
(defconstant KEY_DOWN  #o0402) ; down-arrow key
(defconstant KEY_UP    #o0403) ; up-arrow key
(defconstant KEY_LEFT  #o0404) ; left-arrow key
(defconstant KEY_RIGHT #o0405) ; right-arrow key
(defconstant KEY_ESC   000027) ; Escape key
(defconstant KEY_BACKSPACE0 #o0407) ; backspace key 
(defconstant KEY_BACKSPACE1 127) ; backspace key
(defconstant KEY_BACKSPACE2 263) ; backspace key 
(defconstant KEY_HOME  #o0406) ; home key 
(defconstant KEY_END   #o0550) ; end key 
(defconstant KEY_NPAGE #o0522) ; next-page key
(defconstant KEY_PPAGE #o0523) ;previous-page key
(defconstant KEY_TAB  0009) ; tab key 
(defconstant KEY_F0    #o0410) ; Function keys.  Space for 64 
(defconstant KEY_F1  (+ KEY_F0 1))  ; Value of function key 1
(defconstant KEY_F2  (+ KEY_F0 2))  ; Value of function key 2
(defconstant KEY_F3  (+ KEY_F0 3))  ; Value of function key 3
(defconstant KEY_F4  (+ KEY_F0 4))  ; Value of function key 4
(defconstant KEY_F5  (+ KEY_F0 5))  ; Value of function key 5
(defconstant KEY_F6  (+ KEY_F0 6))  ; Value of function key 6
(defconstant KEY_F7  (+ KEY_F0 7))  ; Value of function key 7
(defconstant KEY_F8  (+ KEY_F0 8))  ; Value of function key 8
(defconstant KEY_F9  (+ KEY_F0 9))  ; Value of function key 9
(defconstant KEY_F10  (+ KEY_F0 10))  ; Value of function key 10
(defconstant KEY_F11  (+ KEY_F0 11))  ; Value of function key 11
(defconstant KEY_F12  (+ KEY_F0 12))  ; Value of function key 12

;; 3 different alternatives for backspace
(defun key_backspace_p(ch) (or (eq ch KEY_BACKSPACE0)
                               (eq ch KEY_BACKSPACE1)
                               (eq ch KEY_BACKSPACE2)))

(defvar *main-window* nil
  "Main ncurses window")

(defvar *message-window* nil
  "1-line window for messages")

(defvar *help-window* nil
  "Top help window")

(shadowing-import 'timeout)
(use-package 'cl-ncurses)

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


(defun handle-key (key)
  (cond ((eq key KEY_UP)
         (message "UP"))
        ((eq key KEY_DOWN)
         (message "DOWN"))
        ((eq key KEY_LEFT) 
         (message "LEFT"))
        ((eq key KEY_RIGHT) 
         (message "RIGHT"))
        ((key_backspace_p key)
         (message "BACKSPACE"))
        ((eq key KEY_HOME) 
         (message "HOME"))
        ((eq key KEY_END) 
         (message "END"))
        ((eq key KEY_TAB) 
         (message "TAB"))
        ((eq key KEY_NPAGE) 
         (message "PGDN"))
        ((eq key KEY_PPAGE) 
         (message "PGUP"))
        ((eq key KEY_F1) 
         (message "F1"))
        ((eq key KEY_F2) 
         (message "F2"))
        ((eq key KEY_F3) 
         (message "F3"))
        ((eq key KEY_F4) 
         (message "F4"))
        ((eq key KEY_F5) 
         (message "F5"))
        ((eq key KEY_F6) 
         (message "F6"))
        ((eq key KEY_F7) 
         (message "F7"))
        ((eq key KEY_F8) 
         (message "F8"))
        ((eq key KEY_F9) 
         (message "F9"))
        ((eq key KEY_F10) 
         (message "F10"))
        ((eq key KEY_F11) 
         (message "F11"))
        ((eq key KEY_F12) 
         (message "F12"))
        (t
         (message (format nil "Pressed: ~a" key))))
  (wrefresh *main-window*))

(defun message (str)
  (wclear *message-window*)
  (mvwprintw *message-window*  0 0 str)
  (wrefresh *message-window*))

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
  (with-ncurses-borderless-win messages-win 1 *cols* (1- *lines*) 0
    (setf *message-window* messages-win)
    ;; create the main window
    (with-ncurses-win win (1- *lines*) *cols* 0 0
        (setf *main-window* win)
      (let ((key nil))
        (loop while (not (eq (setf key (getch)) KEY_ESC)) do
             (handle-key key))))))

(quit)
