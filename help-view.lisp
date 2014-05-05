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
  (:use ::common-lisp :cl-ncurses :zdircmp.util :zdircmp.ui.utils :zdircmp.view.base)
  ;; shadowing refresh from cl-ncurses, we use the one in base-view
  (:shadowing-import-from :zdircmp.view.base :refresh)
  (:export :help-view))


(in-package :zdircmp.view.help)

(defstruct cursor
  "Cursor position. LINE is the line number in window, COLUMN is the column.
Both 0-based"
  (line 0)
  (column 0))


(defclass help-view (view)
  ((cursor-pos :initform (make-cursor)
               :accessor cursor-pos
               :documentation "Current cursor position"))
   (:documentation "Help window class"))


(defgeneric goto-pos (v &key line col))
(defmethod goto-pos ((v help-view) &key (line (cursor-line (cursor-pos v)))
                                     (col (cursor-column (cursor-pos v))))
  (setf (cursor-line (cursor-pos v)) line)
  (setf (cursor-column (cursor-pos v)) col))


(defgeneric print-string (v string &key with-color))
(defmethod print-string ((v help-view) string &key (with-color :white))
  (with-window v w
               (let ((line (cursor-line (cursor-pos v)))
                     (col  (cursor-column (cursor-pos v)))
                     (size (length string)))
                 (with-color-win w with-color
                                 (mvwprintw w line col string))
                 (goto-pos v :col (+ col size)))))

(defmethod refresh :before ((v help-view) &key (force t))
  (declare (ignore force))
  (goto-pos v :line 0 :col 0)
  (print-string v "Directory tree differences report tool.")
  (goto-pos v :line 1 :col 0)
  (print-string v "Navigation: ")
  (print-string v "LEFT/RIGHT/UP/DOWN" :with-color :green)
  (print-string v " and ")
  (print-string v "TAB" :with-color :green)
  (print-string v " to switch between panes")
  (goto-pos v :line 2 :col 0)
  (print-string v "Press ")
  (print-string v "ESC" :with-color :green)
  (print-string v " to exit")
  (goto-pos v :line 3 :col 0)
  (print-string v "Press ")
  (print-string v "ENTER" :with-color :green)
  (print-string v " to open/close directories or start ")  
  (print-string v "vimdiff" :with-color :red)
  (print-string v " on different files")
  (goto-pos v :line 4 :col 0)
  (print-string v "Press ")
  (print-string v "BACKSPACE" :with-color :green)
  (print-string v " to jump up in tree or close current directory")
  (goto-pos v :line 5 :col 0)
  (print-string v "Legend:")
  (goto-pos v :line 6 :col 0)
  (print-string v "\"file name\" - files/dirs are the same")
  (goto-pos v :line 7 :col 0)
  (print-string v "\"file name\"" :with-color :red)
  (print-string v " - files/dirs are different")
  (goto-pos v :line 8 :col 0)
  (print-string v "\"file name\"" :with-color :cyan)
  (print-string v " - files(or contents of dir) exist only on one pane"))
  

#|
  (with-window v w
    (mvwprintw w 0 0 "Directory tree differences report tool.")
    (mvwprintw w 1 0 "Navigation: ")
    (with-color-win w :green
                    (mvwprintw w 1 (length "Navigation: ") "Cursor keys and TAB"))
    (mvwprintw w 2 0 "Press")
    (with-color-win w :green
                    (mvwprintw w 2 (length "Press ") "ESC"))))
    
  |#  





;;; head-view.lisp ends here
