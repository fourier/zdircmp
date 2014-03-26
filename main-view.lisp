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
  (:use ::common-lisp :cl-ncurses :ztree.util :ztree.model.node :ztree.constants)
  ;; import message function from ztree.view.message for easy use of messages
  (:import-from :ztree.view.message :message)
  (:export :create-view
           :destroy-view
           :resize-view
           :process-key
           :set-model-node
           :refresh-view))

(in-package :ztree.view.main)

(shadowing-import 'timeout)

(defstruct ncurses-wrapper window x y width height node)


(defvar *main-window* (make-ncurses-wrapper)
  "Main ncurses window")

(defun destroy-view ()
  (when (ncurses-wrapper-window *main-window*)
    (wborder (ncurses-wrapper-window *main-window*) 32 32 32 32 32 32 32 32)
    (wrefresh (ncurses-wrapper-window *main-window*))
    (delwin (ncurses-wrapper-window *main-window*))
    (setf (ncurses-wrapper-window *main-window*) nil)))

(defun create-view (x y width height)
  (destroy-view)
  (setf (ncurses-wrapper-window *main-window*) (newwin height width y x))
  (setf (ncurses-wrapper-x *main-window*) x)
  (setf (ncurses-wrapper-y *main-window*) y)
  (setf (ncurses-wrapper-width *main-window*) width)
  (setf (ncurses-wrapper-height *main-window*) height)
  (refresh-view))


(defun refresh-view ()
  ;; if we have a ncurses window
  (when (ncurses-wrapper-window *main-window*)
    (wclear (ncurses-wrapper-window *main-window*))
    (box (ncurses-wrapper-window *main-window*) 0 0)
    (wrefresh (ncurses-wrapper-window *main-window*))
    (when (ncurses-wrapper-node *main-window*)
      (refresh-node))))

(defun resize-view (x y width height)
  (wclear (ncurses-wrapper-window *main-window*))
  (wresize (ncurses-wrapper-window *main-window*) height width)
  (mvwin (ncurses-wrapper-window *main-window*) y x)
  (setf (ncurses-wrapper-x *main-window*) x)
  (setf (ncurses-wrapper-y *main-window*) y)
  (setf (ncurses-wrapper-width *main-window*) width)
  (setf (ncurses-wrapper-height *main-window*) height)
  (box (ncurses-wrapper-window *main-window*) 0 0)
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

(defun set-model-node (node)
  (setf (ncurses-wrapper-node *main-window*) node)
  (refresh-view))

(defun refresh-node ()
  (wrefresh (ncurses-wrapper-window *main-window*)))


;;; main-view.cl ends here
