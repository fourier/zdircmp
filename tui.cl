(require 'cl-ncurses)

;; base-8 constants
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
(defconstant KEY_TAB  0009) ; set-tab key 
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

;;(cl-ncurses:newwin 10 10 3 4)
;;(cl-ncurses:delwin 10 10 3 4)
(when t 
  ;; initialize ncurses
  (initscr)
  ;; no caching of the input
                                        ;(cl-ncurses:cbreak)
  ;; turn on special keys 
  (keypad cl-ncurses:*stdscr* 1)
  ;; turn on color 
  (start-color)
  ;; turn off key echoing
  (cl-ncurses:noecho)
  (cl-ncurses:printw "Hello, cl-ncurses")
  (cl-ncurses:mvprintw 1 1 (format nil "Number of rows:    ~a" cl-ncurses:*lines*))
  (cl-ncurses:mvprintw 2 1 (format nil "Number of columns: ~a" cl-ncurses:*cols*))
  (cl-ncurses:refresh)
  (let ((key nil))
    (loop while (not (eq (setf key (cl-ncurses:getch)) KEY_ESC)) do
         (cond ((eq key KEY_UP) 
                (cl-ncurses:mvprintw 3 0 "UP"))
               ((eq key KEY_DOWN) 
                (cl-ncurses:mvprintw 3 0 "DOWN"))
               ((eq key KEY_LEFT) 
                (cl-ncurses:mvprintw 3 0 "LEFT"))
               ((eq key KEY_RIGHT) 
                (cl-ncurses:mvprintw 3 0 "RIGHT"))
               ((key_backspace_p key)
                (cl-ncurses:mvprintw 3 0 "BACKSPACE"))
               ((eq key KEY_HOME) 
                (cl-ncurses:mvprintw 3 0 "HOME"))
               ((eq key KEY_END) 
                (cl-ncurses:mvprintw 3 0 "END"))
               ((eq key KEY_TAB) 
                (cl-ncurses:mvprintw 3 0 "TAB"))
               ((eq key KEY_NPAGE) 
                (cl-ncurses:mvprintw 3 0 "PGDN"))
               ((eq key KEY_PPAGE) 
                (cl-ncurses:mvprintw 3 0 "PGUP"))
               ((eq key KEY_F1) 
                (cl-ncurses:mvprintw 3 0 "F1"))
               ((eq key KEY_F2) 
                (cl-ncurses:mvprintw 3 0 "F2"))
               ((eq key KEY_F3) 
                (cl-ncurses:mvprintw 3 0 "F3"))
               ((eq key KEY_F4) 
                (cl-ncurses:mvprintw 3 0 "F4"))
               ((eq key KEY_F5) 
                (cl-ncurses:mvprintw 3 0 "F5"))
               ((eq key KEY_F6) 
                (cl-ncurses:mvprintw 3 0 "F6"))
               ((eq key KEY_F7) 
                (cl-ncurses:mvprintw 3 0 "F7"))
               ((eq key KEY_F8) 
                (cl-ncurses:mvprintw 3 0 "F8"))
               ((eq key KEY_F9) 
                (cl-ncurses:mvprintw 3 0 "F9"))
               ((eq key KEY_F10) 
                (cl-ncurses:mvprintw 3 0 "F10"))
               ((eq key KEY_F11) 
                (cl-ncurses:mvprintw 3 0 "F11"))
               ((eq key KEY_F12) 
                (cl-ncurses:mvprintw 3 0 "F12"))
               (t
                (cl-ncurses:mvprintw 3 0 (format nil "Pressed: ~a" key))))))
  (cl-ncurses:endwin)
  )
(quit)
