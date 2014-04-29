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
(defpackage :zdircmp.view.message
  (:use ::common-lisp :cl-ncurses :zdircmp.view.base)
  ;; shadowing refresh from cl-ncurses, we use the one in base-view
  (:shadowing-import-from :zdircmp.view.base :refresh)
  (:export :message-view
           :message
           :show-activity
           :with-activity-indicator
           :update-activity))

(require 'cl-ncurses)

(in-package :zdircmp.view.message)

(defclass message-view (view)
  ((last-message :initform nil
                 :accessor last-message
                 :documentation "Last message")
   (activity-indicator-visible :accessor activity-indicator-visible
                               :documentation "Determine if we show the activity indicator")
   (activity-indicator-state :initform 0 :accessor activity-indicator-state
                             :documentation "Activity indicator state, 0-7. Each state represents
one of the following: [-] [\] [|] [/] [-] [\] [|] [/]"))
  (:documentation "1-line window for messages"))


(defparameter +activity-statuses+ (vector "[-]" "[\\]" "[|]" "[/]" "[-]" "[\\]" "[|]" "[/]")
  "possible statuses of the activity indicator")


(defgeneric message (v str &rest arguments)
  (:documentation "Prints a message in a message view"))

(defmethod message ((v message-view) str &rest arguments)
  (let ((msg 
         (if arguments
             (apply #'format nil str arguments)
             str))
        (w (window v)))
    (wclear w)
    (when (activity-indicator-visible v)
      (mvwprintw w 0 0
                 (elt +activity-statuses+ (activity-indicator-state v))))
    (mvwprintw w 0
               (if (activity-indicator-visible v) 4 0)
               msg)
    (setf (last-message v) msg)
    (wrefresh w)))


(defmethod refresh ((v message-view))
 (when (last-message v)
   (message v (last-message v))))


(defgeneric show-activity (v show)
  (:documentation "Show the activity indicator if SHOW is t, hide otherwise"))

(defmethod show-activity ((v message-view) show)
  (setf (activity-indicator-state v) 0)
  (setf (activity-indicator-visible v) show)
  (refresh v))


(defgeneric update-activity (v)
  (:documentation "Update the activity indicator if visible"))

(defmethod update-activity ((v message-view))
  (when (activity-indicator-visible v)
    (setf (activity-indicator-state v) (mod (1+ (activity-indicator-state v)) 8))
    (refresh v)))

(defmacro with-activity-indicator (v &body body)
  "Turn on activity indicator, perform BODY and turn off activity indicator"
  `(progn
     (zdircmp.view.message::show-activity ,v t)
     ,@body
     (zdircmp.view.message::show-activity ,v nil)))

;;; message-view.lisp ends here
