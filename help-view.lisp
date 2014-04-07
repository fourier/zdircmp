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
  (:use ::common-lisp :cl-ncurses)
  (:export :create-view
           :destroy-view
           :resize-view))


(in-package :ztree.view.help)

(shadowing-import 'timeout)
(use-package 'cl-ncurses)


(defvar *help-window* nil
  "Help window")

(defun destroy-view ()
  (when *help-window*
    (delwin *help-window*)
    (setf *help-window* nil)))

(defun create-view (x y width height)
  (destroy-view)
  (setf *help-window* (newwin height width y x))
  (wrefresh *help-window*))

(defun refresh-view ())

(defun resize-view (x y width height)
  (wclear *help-window*)
  (wresize *help-window* height width)
  (mvwin *help-window* y x)
  (refresh-view))



;;; help-view.lisp ends here
