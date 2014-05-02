;;; utils-ui.lisp --- ncurses UI utility functions and macros

;; Copyright (C) 2014 Alexey Veretennikov
;;
;; Author: Alexey Veretennikov <alexey dot veretennikov at gmail dot com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:

;; Main View

;;; Code:
(defpackage :zdircmp.ui.utils
  (:use :common-lisp :cl-ncurses :zdircmp.util)
  (:export :defacs))

(in-package :zdircmp.ui.utils)

;; define ACS_* macros
(defmacro defacs (name char) `(defconstant-export ,name (+ cl-ncurses:a_altcharset
                                                           (char-code ,char))))

;; from ncurses.h
;; ==========================================================
;; VT100 symbols begin here
(defacs ACS_ULCORNER  #\l) ; upper left corner 
(defacs ACS_LLCORNER  #\m) ; lower left corner 
(defacs ACS_URCORNER  #\k) ; upper right corner 
(defacs ACS_LRCORNER  #\j) ; lower right corner 
(defacs ACS_LTEE  #\t) ; tee pointing right 
(defacs ACS_RTEE  #\u) ; tee pointing left 
(defacs ACS_BTEE  #\v) ; tee pointing up 
(defacs ACS_TTEE  #\w) ; tee pointing down 
(defacs ACS_HLINE #\q) ; horizontal line 
(defacs ACS_VLINE #\x) ; vertical line 
(defacs ACS_PLUS  #\n) ; large plus or crossover 
(defacs ACS_S1    #\o) ; scan line 1 
(defacs ACS_S9    #\s) ; scan line 9 
(defacs ACS_DIAMOND #\`) ; diamond 
(defacs ACS_CKBOARD #\a) ; checker board (stipple) 
(defacs ACS_DEGREE  #\f) ; degree symbol 
(defacs ACS_PLMINUS #\g) ; plus/minus 
(defacs ACS_BULLET  #\~) ; bullet 
                                        ; Teletype 5410v1 symbols begin here 
(defacs ACS_LARROW  #\,) ; arrow pointing left 
(defacs ACS_RARROW  #\+) ; arrow pointing right 
(defacs ACS_DARROW  #\.) ; arrow pointing down 
(defacs ACS_UARROW  #\-) ; arrow pointing up 
(defacs ACS_BOARD #\h) ; board of squares 
(defacs ACS_LANTERN #\i) ; lantern symbol 
(defacs ACS_BLOCK #\0) ; solid square block 


;; These aren't documented, but a lot of System Vs have them anyway
;; (you can spot pprryyzz{{||}} in a lot of AT&T terminfo strings).
;; The ACS_names may not match AT&T's, our source didn't know them.

(defacs ACS_S3    #\p) ; scan line 3 
(defacs ACS_S7    #\r) ; scan line 7 
(defacs ACS_LEQUAL  #\y) ; less/equal 
(defacs ACS_GEQUAL  #\z) ; greater/equal 
(defacs ACS_PI    #\{) ; Pi 
(defacs ACS_NEQUAL  #\|) ; not equal 
(defacs ACS_STERLING  #\}) ; UK pound sign 


;; Line drawing ACS names are of the form ACS_trbl, where t is the top, r
;; is the right, b is the bottom, and l is the left.  t, r, b, and l might
;; be B (blank), S (single), D (double), or T (thick).  The subset defined
;; here only uses B and S.

(defconstant-export ACS_BSSB  ACS_ULCORNER)
(defconstant-export ACS_SSBB  ACS_LLCORNER)
(defconstant-export ACS_BBSS  ACS_URCORNER)
(defconstant-export ACS_SBBS  ACS_LRCORNER)
(defconstant-export ACS_SBSS  ACS_RTEE)
(defconstant-export ACS_SSSB  ACS_LTEE)
(defconstant-export ACS_SSBS  ACS_BTEE)
(defconstant-export ACS_BSSS  ACS_TTEE)
(defconstant-export ACS_BSBS  ACS_HLINE)
(defconstant-export ACS_SBSB  ACS_VLINE)
(defconstant-export ACS_SSSS  ACS_PLUS)



(defmacro-export with-ncurses (&body body)
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


(defmacro-export with-ncurses-win (win lines cols y x &body body )
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

(defmacro-export with-ncurses-borderless-win (win lines cols y x &body body )
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



(defmacro-export with-color-win (win color &body body)
  `(let ((color-value
          (case ,color
            (:white '1)
            (:red '2)
            (:green '3)
            (:blue '4)
            (:yellow '5)
            (:magenta '6)
            (:cyan '7)
            (:black-on-white '8)
            (:red-on-white '9)
            (:green-on-white '10)
            (:blue-on-white '11)
            (:cyan-on-blue '12)
            (otherwise '1))))
     (progn
       (cl-ncurses:wattron ,win (cl-ncurses:COLOR-PAIR color-value))
       ,@body
       (cl-ncurses:wattron ,win (cl-ncurses:COLOR-PAIR 1)))))


(defun-export init-color-pairs ()
  ;; create the color pairs
  ;; create color pairs
  ;; foregrounds
  (cl-ncurses:init-pair 1 cl-ncurses:COLOR_WHITE cl-ncurses:COLOR_BLACK)
  (cl-ncurses:init-pair 2 cl-ncurses:COLOR_RED cl-ncurses:COLOR_BLACK)
  (cl-ncurses:init-pair 3 cl-ncurses:COLOR_GREEN cl-ncurses:COLOR_BLACK)
  (cl-ncurses:init-pair 4 cl-ncurses:COLOR_BLUE cl-ncurses:COLOR_BLACK)
  (cl-ncurses:init-pair 5 cl-ncurses:COLOR_YELLOW cl-ncurses:COLOR_BLACK)
  (cl-ncurses:init-pair 6 cl-ncurses:COLOR_MAGENTA cl-ncurses:COLOR_BLACK)
  (cl-ncurses:init-pair 7 cl-ncurses:COLOR_CYAN cl-ncurses:COLOR_BLACK)
  ;; backgrounds
  (cl-ncurses:init-pair 8 cl-ncurses:COLOR_BLACK cl-ncurses:COLOR_WHITE)
  (cl-ncurses:init-pair 9 cl-ncurses:COLOR_RED cl-ncurses:COLOR_WHITE)
  (cl-ncurses:init-pair 10 cl-ncurses:COLOR_GREEN cl-ncurses:COLOR_WHITE)
  (cl-ncurses:init-pair 11 cl-ncurses:COLOR_BLUE cl-ncurses:COLOR_WHITE)
  (cl-ncurses:init-pair 12 cl-ncurses:COLOR_CYAN cl-ncurses:COLOR_BLUE))



;;; utils-ui.lisp ends here
