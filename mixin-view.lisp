;;; mixin-view.lisp --- interface (mixin) class for TUI view (window) 

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

;; View interface

;;; Code:
(defpackage :zdircmp.view.mixin
  (:use ::common-lisp)
  ;; shadowing refresh from cl-ncurses, we don't use it anyway
  (:export :mixin
           :window
           :window-rect
           :refresh
           :destroy
           :resize
           :visible
           :show
           :process-key))

(in-package :zdircmp.view.mixin)


(defclass mixin ()
  ()
  (:documentation "Interface class for ncurses-based views"))


(defgeneric window (v)
  (:documentation "NCurses window"))

(defgeneric window-rect (v)
  (:documentation "Non-client window rect in screen coordinate system"))

(defgeneric destroy (v)
  (:documentation "Destroy the associated with view ncurses window"))

(defgeneric refresh (v &key force)
  (:documentation "Refreshes the associated ncurses window"))

(defgeneric resize (v x y width height)
  (:documentation "Process the resize command, resizing the associated ncurses window"))

(defgeneric visible (v)
  (:documentation "Determines if the window is visible"))

(defgeneric show (v show)
  (:documentation "Show or hides window depending on SHOW argument"))

(defgeneric process-key (v key)
  (:documentation "Key handler for view"))

;;; mixin-view.lisp ends here
