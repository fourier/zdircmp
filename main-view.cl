;;; main-view.cl --- main TUI view (window) for directory trees

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
(defpackage :ztree.view.main
  (:use ::common-lisp :ztree.util :ztree.diff.model :ztree.constants)
  (:export :create-view
           :destroy-view
           :resize-view
           :process-key
           :refresh-view))

(require 'cl-ncurses)

(in-package :ztree.view.main)

(shadowing-import 'timeout)
(use-package 'cl-ncurses)

;; import message function from ztree.view.message for easy use of messages
(import 'ztree.view.message::message)


(defvar *main-window* nil
  "Main ncurses window")

(defun destroy-view ()
  (when *main-window*
    (wborder *main-window* 32 32 32 32 32 32 32 32)
    (wrefresh *main-window*)
    (delwin *main-window*)
    (setf *main-window* nil)))

(defun create-view (x y width height)
  (destroy-view)
  (setf *main-window* (newwin height width y x))
  (box *main-window* 0 0)
  (wrefresh *main-window*))



(defun refresh-view ()
  (if *main-window*
      (wrefresh *main-window*)))

(defun resize-view (x y width height)
  (wclear *main-window*)
  (wresize *main-window* height width)
  (mvwin *main-window* y x)
  (box *main-window* 0 0)
  (refresh-view))


(defun process-key (key)
  (cond ((eq key +KEY-UP+)
         (message "UP"))
        ((eq key +KEY-DOWN+)
         (message "DOWN"))
        ((eq key +KEY-LEFT+) 
         (message "LEFT"))
        ((eq key +KEY-RIGHT+) 
         (message "RIGHT"))
        ((eq key +KEY-ENTER+) 
         (message "ENTER"))
        ((eq key +KEY-SPACE+) 
         (message "SPACE"))
        ((key-backspace-p key)
         (message "BACKSPACE"))
        ((eq key +KEY-HOME+) 
         (message "HOME"))
        ((eq key +KEY-END+) 
         (message "END"))
        ((eq key +KEY-TAB+) 
         (message "TAB"))
        ((eq key +KEY-NPAGE+) 
         (message "PGDN"))
        ((eq key +KEY-PPAGE+) 
         (message "PGUP"))
        ((eq key +KEY-F1+) 
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
        (t
         (message (format nil "Pressed: ~a" key))))
  (refresh-view))


;;; main-view.cl ends here
