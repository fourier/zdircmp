;;; head-view.lisp --- Help text view (window) for directory trees

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
(defpackage :zdircmp.view.help
  (:use ::common-lisp :cl-ncurses :zdircmp.util)
  (:export :create-view
           :destroy-view
           :resize-view
           :show-view))


(in-package :zdircmp.view.help)

(shadowing-import 'timeout)
(use-package 'cl-ncurses)

(defmacro with-color (color &body body)
  "Shortcut for with-color-win for currert window"
  `(with-color-win (main-window-window *head-window*) ,color ,@body))


(defvar *head-window* nil
  "Help window")

(defstruct head-window
  (window nil)
  (x 0)
  (y 0)
  (width 0)
  (height 0)
  (left-path nil)
  (right-path nil))


(defun destroy-view ()
  (when *head-window*
    (let ((nwin (head-window-window *head-window*)))
      (when nwin
        (wclear nwin)
        (wrefresh nwin)
        (delwin nwin)))
    (setf *head-window* nil)))

(defun create-view (x y width height left-path right-path)
  (destroy-view)
  (setf *head-window* (make-head-window :window (newwin height width y x)
                                        :x x
                                        :y y
                                        :height height
                                        :width width
                                        :left-path left-path
                                        :right-path right-path))
  (refresh-view))


(defun refresh-view ()
  (let ((nwin (head-window-window *head-window*)))
    (when nwin 
      (wclear nwin)
      (mvwprintw nwin 0 0 "Directory tree differences report")
      (mvwprintw nwin 1 0 (concat "Left:  " (head-window-left-path *head-window*)))
      (mvwprintw nwin 2 0 (concat "Right: " (head-window-right-path *head-window*))))
    (wrefresh nwin)))

(defun resize-view (x y width height)
  (when *head-window*
    (let ((nwin (head-window-window *head-window*)))
      (wclear nwin)
      (wresize nwin height width)
      (mvwin nwin y x)
      (setf (head-window-x *head-window*) x)
      (setf (head-window-y *head-window*) y)
      (setf (head-window-width *head-window*) width)
      (setf (head-window-height *head-window*) height)
      (refresh-view))))

(defun show-view (show)
  (let ((nwin (head-window-window *head-window*)))
    (when nwin
      (wclear nwin)
      (wrefresh nwin)
      (delwin nwin))
    (setf (head-window-window *head-window*) 
          (if show (newwin (head-window-height *head-window*)
                           (head-window-width *head-window*)
                           (head-window-y *head-window*)
                           (head-window-x *head-window*))
              nil))
    (refresh-view)))




;;; head-view.lisp ends here
