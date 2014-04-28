;;; command-ui.lisp --- Last entered command storage

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

;;; Code:

(defpackage :zdircmp.ui.command
  (:use :common-lisp)
  (:export :user-command
           :last-command
           :set-last-command
           :repeat-count
           ))

(in-package :zdircmp.ui.command)


;; Last entered user command
(defclass user-command ()
  ((last-command :initform nil :reader last-command :writer set-last-command)
   (repeat-count :initform 0 :reader repeat-count)))

(defmethod set-last-command (cmd (self user-command))
  (with-slots (last-command repeat-count) self
    (setf repeat-count 
          (if (eq last-command cmd) (1+ repeat-count) 0))
    (setf last-command cmd)))

;;; command-ui.lisp ends here
