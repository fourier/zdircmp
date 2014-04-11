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
  (:use :common-lisp :ztree.util))

(in-package :ztree.constants)


;; base-8 constants for getch, from curses.h
(defconstant-export +KEY-DOWN+  #o0402) ; down-arrow key
(defconstant-export +KEY-UP+    #o0403) ; up-arrow key
(defconstant-export +KEY-LEFT+  #o0404) ; left-arrow key
(defconstant-export +KEY-RIGHT+ #o0405) ; right-arrow key
(defconstant-export +KEY-ESC+   000027) ; Escape key
(defconstant-export +KEY-BACKSPACE0+ #o0407) ; backspace key 
(defconstant-export +KEY-BACKSPACE1+ 127) ; backspace key
(defconstant-export +KEY-BACKSPACE2+ 263) ; backspace key 
(defconstant-export +KEY-HOME+  #o0406) ; home key 
(defconstant-export +KEY-END+   #o0550) ; end key 
(defconstant-export +KEY-NPAGE+ #o0522) ; next-page key
(defconstant-export +KEY-PPAGE+ #o0523) ;previous-page key
(defconstant-export +KEY-TAB+  0009) ; tab key
(defconstant-export +KEY-ENTER+ 10) ; enter/send key
(defconstant-export +KEY-SPACE+ 32) ; space key
(defconstant-export +KEY-F0+    #o0410) ; Function keys.  Space for 64 
(defconstant-export +KEY-F1+  (+ +KEY-F0+ 1))  ; Value of function key 1
(defconstant-export +KEY-F2+  (+ +KEY-F0+ 2))  ; Value of function key 2
(defconstant-export +KEY-F3+  (+ +KEY-F0+ 3))  ; Value of function key 3
(defconstant-export +KEY-F4+  (+ +KEY-F0+ 4))  ; Value of function key 4
(defconstant-export +KEY-F5+  (+ +KEY-F0+ 5))  ; Value of function key 5
(defconstant-export +KEY-F6+  (+ +KEY-F0+ 6))  ; Value of function key 6
(defconstant-export +KEY-F7+  (+ +KEY-F0+ 7))  ; Value of function key 7
(defconstant-export +KEY-F8+  (+ +KEY-F0+ 8))  ; Value of function key 8
(defconstant-export +KEY-F9+  (+ +KEY-F0+ 9))  ; Value of function key 9
(defconstant-export +KEY-F10+  (+ +KEY-F0+ 10))  ; Value of function key 10
(defconstant-export +KEY-F11+  (+ +KEY-F0+ 11))  ; Value of function key 11
(defconstant-export +KEY-F12+  (+ +KEY-F0+ 12))  ; Value of function key 12

;; 3 different alternatives for backspace
(defun-export key-backspace-p(ch) (or (eq ch +KEY-BACKSPACE0+)
                                      (eq ch +KEY-BACKSPACE1+)
                                      (eq ch +KEY-BACKSPACE2+)))

;;; constants.lisp ends here
