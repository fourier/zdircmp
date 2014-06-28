;;; mixin-activity.lisp --- interface (mixin) class for activity indication 

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

;; interface for the activity indication notifier

;;; Code:
(defpackage :zdircmp.ui.activity
  (:use ::common-lisp)
  ;; shadowing refresh from cl-ncurses, we don't use it anyway
  (:export :activity-mixin
           :update-activity))

(in-package :zdircmp.ui.activity)


(defclass activity-mixin ()
  ()
  (:documentation "Interface class for the progress(activity) indication"))

(defgeneric update-activity (v)
  (:documentation "Update the activity indicator if visible"))


;;; mixin-activity.lisp ends here
