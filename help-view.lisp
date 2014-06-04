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
  (:use ::common-lisp
        :cl-ncurses
        :zdircmp.util
        :zdircmp.ui.utils
        :zdircmp.view.base)
  ;; shadowing refresh from cl-ncurses, we use the one in base-view
  (:shadowing-import-from :zdircmp.view.base :refresh)
  (:export :make-help-view))


(in-package :zdircmp.view.help)


(defclass help-view (view)
  ((difftool :initarg :difftool :initform "vimdiff" :accessor difftool))
  (:documentation "Help window class"))


(defun make-help-view (x y width height)
  (make-instance 'help-view
                 :window-rect
                 (make-rect
                  :x x
                  :y y
                  :width width
                  :height height)))


(defmethod refresh ((v help-view) &key (force t))
  (declare (ignore force))
  (goto-point v :line 0 :col 0)
  (text-out v "Directory tree differences report tool.")
  (goto-point v :line 1 :col 0)
  (text-out v "Navigation: ")
  (text-out v "LEFT/RIGHT/UP/DOWN" :with-color :green)
  (text-out v " and ")
  (text-out v "TAB" :with-color :green)
  (text-out v " to switch between panes")
  (goto-point v :line 2 :col 0)
  (text-out v "Press ")
  (text-out v "ESC" :with-color :green)
  (text-out v " to exit")
  (goto-point v :line 3 :col 0)
  (text-out v "Press ")
  (text-out v "ENTER" :with-color :green)
  (text-out v " to open/close directories or start ")  
  (text-out v (difftool v) :with-color :red)
  (text-out v " on different files")
  (goto-point v :line 4 :col 0)
  (text-out v "Press ")
  (text-out v "BACKSPACE" :with-color :green)
  (text-out v " to jump up in tree or close current directory")
  (goto-point v :line 5 :col 0)
  (text-out v "Legend:")
  (goto-point v :line 6 :col 0)
  (text-out v "\"file name\" - files/dirs are the same")
  (goto-point v :line 7 :col 0)
  (text-out v "\"file name\"" :with-color :red)
  (text-out v " - files/dirs are different")
  (goto-point v :line 8 :col 0)
  (text-out v "\"file name\"" :with-color :cyan)
  (text-out v " - files(or contents of dir) exist only on one pane")
  (call-next-method))

;;; head-view.lisp ends here
