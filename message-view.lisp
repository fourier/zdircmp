;;; message-view.lisp --- message TUI view (window) for directory trees

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

;; Message View

;;; Code:
(defpackage :ztree.view.message
  (:use ::common-lisp :cl-ncurses)
  (:export :create-view
           :destroy-view
           :resize-view
           :message))

(require 'cl-ncurses)

(in-package :ztree.view.message)

(shadowing-import 'timeout)


(defvar *message-window* nil
  "1-line window for messages")

(defvar *last-message* nil
  "Last message shown in window")

(defun destroy-view ()
  (when *message-window*
    (delwin *message-window*)
    (setf *message-window* nil)))

(defun create-view (x y width height)
  (destroy-view)
  (setf *message-window* (newwin height width y x))
  (wrefresh *message-window*))

(defun message (str)
  (when *message-window*
    (wclear *message-window*)
    (mvwprintw *message-window*  0 0 str)
    (setf *last-message* str)
    (wrefresh *message-window*)))


(defun refresh-view ()
  (message *last-message*))

(defun resize-view (x y width height)
  (wclear *message-window*)
  (wresize *message-window* height width)
  (mvwin *message-window* y x)
  (refresh-view))



;;; message-view.lisp ends here
