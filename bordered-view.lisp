;;; bordered-view.lisp --- base class for TUI view with borders

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

;; View With Borders

;;; Code:
(defpackage :zdircmp.view.bordered
  (:use ::common-lisp :cl-ncurses :zdircmp.ui.utils :zdircmp.view.base)
  (:shadowing-import-from :zdircmp.view.base :refresh)
  (:export :bordered-view))

(in-package :zdircmp.view.bordered)

(defclass bordered-view (view)
  ()
  (:documentation "Base class for ncurses-based views with borders"))

(defmethod refresh :before ((v bordered-view) &key force)
  (declare (ignore force))
  (with-window (v w)
    (box w 0 0)))

(defmethod initialize-instance :after ((v bordered-view) &rest args)
  ;; ignore unused args warning
  (declare (ignore args))
  (update-client-rect v))

(defmethod update-client-rect ((v bordered-view))
  (with-slots (window-rect client-rect) v
    (setf client-rect (make-rect :x 1
                                 :y 1
                                 :width (- (rect-width window-rect) 2)
                                 :height (- (rect-height window-rect) 2)))))


                                 
;;; bordered-view.lisp ends here
