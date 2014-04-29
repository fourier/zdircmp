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
  (:use ::common-lisp :cl-ncurses :zdircmp.util :zdircmp.view.base)
  ;; shadowing refresh from cl-ncurses, we use the one in base-view
  (:shadowing-import-from :zdircmp.view.base :refresh)
  (:export :help-view))


(in-package :zdircmp.view.help)

(defclass help-view (view)
  ((left-path :initarg :left-path
              :initform nil
              :accessor left-path
              :documentation "Path for the left pane")
   (right-path :initarg :right-path
               :initform nil
               :accessor right-path
               :documentation "Path for the right pane"))
   (:documentation "Help window class"))


(defmethod refresh :before ((v help-view))
  (let ((w (window v)))
    (mvwprintw w 0 0 "Directory tree differences report")
    (mvwprintw w 1 0 (concat "Left:  " (left-path v)))
    (mvwprintw w 2 0 (concat "Right: " (right-path v)))))
    






;;; head-view.lisp ends here
