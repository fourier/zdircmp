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
  (:use :common-lisp)
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

;; export all symbols from ztree.constants
(let ((pack (find-package :ztree.constants)))
  (do-all-symbols (sym pack) (when (eql (symbol-package sym) pack) (export sym))))

;;; constants.lisp ends here
