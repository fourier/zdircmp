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



(defmacro with-ncurses-win (win lines cols y x &body body)
  "Macro to create and operate with the ncurses-window.
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


(with-ncurses
  ;; no caching of the input
  (cbreak)
  ;; turn on special keys 
  (keypad *stdscr* 1)
  ;; turn on color 
  (start-color)
  ;; turn off key echoing
  (noecho)
  ;; create the main window
  (with-ncurses-win win *lines* *cols* 0 0
    (let ((key nil)
          (x 1)
          (y 1))
      (loop while (not (eq (setf key (getch)) KEY_ESC)) do
           (cond ((eq key KEY_UP) 
                  (mvwprintw win y x "UP"))
                 ((eq key KEY_DOWN) 
                  (mvwprintw win y x "DOWN"))
                 ((eq key KEY_LEFT) 
                  (mvwprintw win y x "LEFT"))
                 ((eq key KEY_RIGHT) 
                  (mvwprintw win y x "RIGHT"))
                 ((key_backspace_p key)
                  (mvwprintw win y x "BACKSPACE"))
                 ((eq key KEY_HOME) 
                  (mvwprintw win y x "HOME"))
                 ((eq key KEY_END) 
                  (mvwprintw win y x "END"))
                 ((eq key KEY_TAB) 
                  (mvwprintw win y x "TAB"))
                 ((eq key KEY_NPAGE) 
                  (mvwprintw win y x "PGDN"))
                 ((eq key KEY_PPAGE) 
                  (mvwprintw win y x "PGUP"))
                 ((eq key KEY_F1) 
                  (mvwprintw win y x "F1"))
                 ((eq key KEY_F2) 
                  (mvwprintw win y x "F2"))
                 ((eq key KEY_F3) 
                  (mvwprintw win y x "F3"))
                 ((eq key KEY_F4) 
                  (mvwprintw win y x "F4"))
                 ((eq key KEY_F5) 
                  (mvwprintw win y x "F5"))
                 ((eq key KEY_F6) 
                  (mvwprintw win y x "F6"))
                 ((eq key KEY_F7) 
                  (mvwprintw win y x "F7"))
                 ((eq key KEY_F8) 
                  (mvwprintw win y x "F8"))
                 ((eq key KEY_F9) 
                  (mvwprintw win y x "F9"))
                 ((eq key KEY_F10) 
                  (mvwprintw win y x "F10"))
                 ((eq key KEY_F11) 
                  (mvwprintw win y x "F11"))
                 ((eq key KEY_F12) 
                  (mvwprintw win y x "F12"))
                 (t
                  (mvwprintw win y x (format nil "Pressed: ~a" key))))
               (wrefresh win)))))


(when nil
  (printw "Hello, cl-ncurses")
  (mvprintw 1 1 (format nil "Number of rows:    ~a" *lines*))
  (mvprintw 2 1 (format nil "Number of columns: ~a" *cols*))
  (refresh)
  (let ((key nil))
    (loop while (not (eq (setf key (getch)) KEY_ESC)) do
         (cond ((eq key KEY_UP) 
                (mvprintw 3 0 "UP"))
               ((eq key KEY_DOWN) 
                (mvprintw 3 0 "DOWN"))
               ((eq key KEY_LEFT) 
                (mvprintw 3 0 "LEFT"))
               ((eq key KEY_RIGHT) 
                (mvprintw 3 0 "RIGHT"))
               ((key_backspace_p key)
                (mvprintw 3 0 "BACKSPACE"))
               ((eq key KEY_HOME) 
                (mvprintw 3 0 "HOME"))
               ((eq key KEY_END) 
                (mvprintw 3 0 "END"))
               ((eq key KEY_TAB) 
                (mvprintw 3 0 "TAB"))
               ((eq key KEY_NPAGE) 
                (mvprintw 3 0 "PGDN"))
               ((eq key KEY_PPAGE) 
                (mvprintw 3 0 "PGUP"))
               ((eq key KEY_F1) 
                (mvprintw 3 0 "F1"))
               ((eq key KEY_F2) 
                (mvprintw 3 0 "F2"))
               ((eq key KEY_F3) 
                (mvprintw 3 0 "F3"))
               ((eq key KEY_F4) 
                (mvprintw 3 0 "F4"))
               ((eq key KEY_F5) 
                (mvprintw 3 0 "F5"))
               ((eq key KEY_F6) 
                (mvprintw 3 0 "F6"))
               ((eq key KEY_F7) 
                (mvprintw 3 0 "F7"))
               ((eq key KEY_F8) 
                (mvprintw 3 0 "F8"))
               ((eq key KEY_F9) 
                (mvprintw 3 0 "F9"))
               ((eq key KEY_F10) 
                (mvprintw 3 0 "F10"))
               ((eq key KEY_F11) 
                (mvprintw 3 0 "F11"))
               ((eq key KEY_F12) 
                (mvprintw 3 0 "F12"))
               (t
                (mvprintw 3 0 (format nil "Pressed: ~a" key))))))
  (endwin)
  )
(quit)
