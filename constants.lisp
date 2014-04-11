;;; constants.lisp --- constants defined in the project

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
(defpackage :ztree.constants
  (:use :common-lisp :cl-ncurses)
  (:export :key-backspace-p))

(in-package :ztree.constants)

;; base-8 constants for getch, from curses.h
(defconstant +KEY-DOWN+  #o0402) ; down-arrow key
(defconstant +KEY-UP+    #o0403) ; up-arrow key
(defconstant +KEY-LEFT+  #o0404) ; left-arrow key
(defconstant +KEY-RIGHT+ #o0405) ; right-arrow key
(defconstant +KEY-ESC+   000027) ; Escape key
(defconstant +KEY-BACKSPACE0+ #o0407) ; backspace key 
(defconstant +KEY-BACKSPACE1+ 127) ; backspace key
(defconstant +KEY-BACKSPACE2+ 263) ; backspace key 
(defconstant +KEY-HOME+  #o0406) ; home key 
(defconstant +KEY-END+   #o0550) ; end key 
(defconstant +KEY-NPAGE+ #o0522) ; next-page key
(defconstant +KEY-PPAGE+ #o0523) ;previous-page key
(defconstant +KEY-TAB+  0009) ; tab key
(defconstant +KEY-ENTER+ 10) ; enter/send key
(defconstant +KEY-SPACE+ 32) ; space key
(defconstant +KEY-F0+    #o0410) ; Function keys.  Space for 64 
(defconstant +KEY-F1+  (+ +KEY-F0+ 1))  ; Value of function key 1
(defconstant +KEY-F2+  (+ +KEY-F0+ 2))  ; Value of function key 2
(defconstant +KEY-F3+  (+ +KEY-F0+ 3))  ; Value of function key 3
(defconstant +KEY-F4+  (+ +KEY-F0+ 4))  ; Value of function key 4
(defconstant +KEY-F5+  (+ +KEY-F0+ 5))  ; Value of function key 5
(defconstant +KEY-F6+  (+ +KEY-F0+ 6))  ; Value of function key 6
(defconstant +KEY-F7+  (+ +KEY-F0+ 7))  ; Value of function key 7
(defconstant +KEY-F8+  (+ +KEY-F0+ 8))  ; Value of function key 8
(defconstant +KEY-F9+  (+ +KEY-F0+ 9))  ; Value of function key 9
(defconstant +KEY-F10+  (+ +KEY-F0+ 10))  ; Value of function key 10
(defconstant +KEY-F11+  (+ +KEY-F0+ 11))  ; Value of function key 11
(defconstant +KEY-F12+  (+ +KEY-F0+ 12))  ; Value of function key 12

;; 3 different alternatives for backspace
(defun key-backspace-p(ch) (or (eq ch +KEY-BACKSPACE0+)
                               (eq ch +KEY-BACKSPACE1+)
                               (eq ch +KEY-BACKSPACE2+)))

;; define ACS_* macros
(defmacro defacs (name char) `(defconstant ,name (+ cl-ncurses:a_altcharset
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

(defconstant ACS_BSSB  ACS_ULCORNER)
(defconstant ACS_SSBB  ACS_LLCORNER)
(defconstant ACS_BBSS  ACS_URCORNER)
(defconstant ACS_SBBS  ACS_LRCORNER)
(defconstant ACS_SBSS  ACS_RTEE)
(defconstant ACS_SSSB  ACS_LTEE)
(defconstant ACS_SSBS  ACS_BTEE)
(defconstant ACS_BSSS  ACS_TTEE)
(defconstant ACS_BSBS  ACS_HLINE)
(defconstant ACS_SBSB  ACS_VLINE)
(defconstant ACS_SSSS  ACS_PLUS)

;; export all symbols from ztree.constants
(let ((pack (find-package :ztree.constants)))
  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))

;;; constants.lisp ends here
