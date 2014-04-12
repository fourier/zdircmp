;;; help-view.lisp --- Help text view (window) for directory trees

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

;; Help View

;;; Code:
(defpackage :ztree.view.help
  (:use ::common-lisp :cl-ncurses :ztree.util)
  (:export :create-view
           :destroy-view
           :resize-view
           :show-view))


(in-package :ztree.view.help)

(shadowing-import 'timeout)
(use-package 'cl-ncurses)

(defmacro with-color (color &body body)
  "Shortcut for with-color-win for currert window"
  `(with-color-win (main-window-window *help-window*) ,color ,@body))


(defvar *help-window* nil
  "Help window")

(defstruct help-window
  (window nil)
  (x 0)
  (y 0)
  (width 0)
  (height 0)
  (left-path nil)
  (right-path nil))


(defun destroy-view ()
  (when *help-window*
    (let ((nwin (help-window-window *help-window*)))
      (when nwin
        (wclear nwin)
        (wrefresh nwin)
        (delwin nwin)))
    (setf *help-window* nil)))

(defun create-view (x y width height left-path right-path)
  (destroy-view)
  (setf *help-window* (make-help-window :window (newwin height width y x)
                                        :x x
                                        :y y
                                        :height height
                                        :width width
                                        :left-path left-path
                                        :right-path right-path))
  (refresh-view))


(defun refresh-view ()
  (let ((nwin (help-window-window *help-window*)))
    (when nwin 
      (wclear nwin)
      (when t;(and (help-window-left-path *help-window*)
            ;     (help-window-right-path *help-window*))
        (mvwprintw nwin 0 0 "Directory tree differences report")
        (mvwprintw nwin 1 0 (concat "Left:  " (help-window-left-path *help-window*)))
        (mvwprintw nwin 2 0 (concat "Right: " (help-window-right-path *help-window*))))
      (wrefresh nwin))))

(defun resize-view (x y width height)
  (when *help-window*
    (let ((nwin (help-window-window *help-window*)))
      (wclear nwin)
      (wresize nwin height width)
      (mvwin nwin y x)
      (setf (help-window-x *help-window*) x)
      (setf (help-window-y *help-window*) y)
      (setf (help-window-width *help-window*) width)
      (setf (help-window-height *help-window*) height)
      (refresh-view))))

(defun show-view (show)
  (let ((nwin (help-window-window *help-window*)))
    (if show
        (progn
          (when nwin
            (wclear nwin)
            (wrefresh nwin)
            (delwin nwin))
          (setf (help-window-window *help-window*) 
                (newwin (help-window-height *help-window*)
                        (help-window-width *help-window*)
                        (help-window-y *help-window*)
                        (help-window-x *help-window*))))
        (progn
          (when nwin
            (wclear nwin)
            (wrefresh nwin)
            (delwin nwin))
          (setf (help-window-window *help-window*) nil))))
  (refresh-view))

  
  

;;; help-view.lisp ends here
