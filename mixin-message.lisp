;;; mixin-message.lisp --- interface (mixin) class for 1-line messages 

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

;; 1-line message interface (status line etc)

;;; Code:
(defpackage :zdircmp.ui.message
  (:use ::common-lisp)
  ;; shadowing refresh from cl-ncurses, we don't use it anyway
  (:export :message-mixin
           :message))

(in-package :zdircmp.ui.message)


(defclass message-mixin ()
  ()
  (:documentation "Interface class for 1-line message window (status etc)"))

(defgeneric message (v str &rest arguments)
  (:documentation "Prints a message through serial, stdout, log, separate window etc."))


;;; mixin-message.lisp ends here
