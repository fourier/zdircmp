;;; status-view.lisp --- 1-line status view (window) for directory trees

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
(defpackage :zdircmp.view.status
  (:use ::common-lisp
        :cl-ncurses
        :zdircmp.view.base
        :zdircmp.util
        :zdircmp.ui.utils)
  ;; shadowing refresh from cl-ncurses, we use the one in base-view
  (:shadowing-import-from :zdircmp.view.base :refresh)
  (:export :make-status-view))

(in-package :zdircmp.view.status)

(defclass status-view (view)
  ((left-path :initarg :left-path
              :initform nil
              :accessor left-path
              :documentation "Path for the left pane")
   (right-path :initarg :right-path
               :initform nil
               :accessor right-path
               :documentation "Path for the right pane"))
  (:documentation "1-line status view"))

(defun make-status-view (x y width height left-path right-path)
  (make-instance 'status-view
                 :window-rect
                 (make-rect
                  :x x
                  :y y
                  :width width
                  :height height)
                 :left-path left-path
                 :right-path right-path))

(defmethod refresh ((v status-view) &key (force t))
  (declare (ignore force))
  (with-window (v w)
    ;; we need to get the format string like:
    ;; (format nil "~30,,,'-@<---zdircmp: ~a/~a ~>" "dir1" "dir2")
    ;; where 30 is the screen width
    (let* ((format-string (concat "~"
                                  (format nil "~d" (width v))
                                  ",,,'-@<---zdircmp: ~a/~a ~>"))
           (msg (format nil format-string
                        (file-short-name (left-path v))
                        (file-short-name (right-path v)))))
      (text-out v msg :with-color :cyan-on-blue :line 0 :col 0))
    (wrefresh w)))

;;; status-view.lisp ends here
