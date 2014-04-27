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
(defpackage :ztree.view.message
  (:use ::common-lisp :cl-ncurses :ztree.view.base)
  ;; shadowing refresh from cl-ncurses, we use the one in base-view
  (:shadow :refresh)
  (:export :create-view
           :destroy-view
           :resize-view
           :message
           :show-activity
           :with-activity-indicator
           :update-activity))

(require 'cl-ncurses)

(in-package :ztree.view.message)


(defvar *message-window* nil
  "1-line window for messages")

(defvar *last-message* nil
  "Last message shown in window")

(defvar *activity-indicator-visible* nil
  "Determine if we show the activity indicator")

(defvar *activity-indicator-state* 0
  "Activity indicator state, 0-7. Each state represents
one of the following: [-] [\] [|] [/] [-] [\] [|] [/]")

(defparameter +activity-statuses+ (vector "[-]" "[\\]" "[|]" "[/]" "[-]" "[\\]" "[|]" "[/]")
  "possible statuses of the activity indicator")

(defun destroy-view ()
  (when *message-window*
    (delwin *message-window*)
    (setf *message-window* nil)))

(defun create-view (x y width height)
  (destroy-view)
  (setf *message-window* (newwin height width y x))
  (wrefresh *message-window*))


(defun message (str &rest arguments)
  (let ((msg 
         (if arguments
             (apply #'format nil str arguments)
             str)))
    (when *message-window*
      (wclear *message-window*)
      (when *activity-indicator-visible*
        (mvwprintw *message-window* 0 0
                   (elt +activity-statuses+ *activity-indicator-state*)))
      (mvwprintw *message-window* 0
                 (if *activity-indicator-visible* 4 0)
                 msg)
      (setf *last-message* msg)
      (wrefresh *message-window*))))


(defun refresh-view ()
  (when *last-message*
    (message *last-message*)))

(defun resize-view (x y width height)
  (wclear *message-window*)
  (wresize *message-window* height width)
  (mvwin *message-window* y x)
  (refresh-view))


(defun show-activity (show)
  "Show the activity indicator if SHOW is t, hide otherwise"
  (setf *activity-indicator-state* 0)
  (setf *activity-indicator-visible* show)
  (refresh-view))


(defun update-activity ()
  "Update the activity indicator if visible"
  (when *activity-indicator-visible*
    (setf *activity-indicator-state* (mod (1+ *activity-indicator-state*) 8))
    (refresh-view)))


(defmacro with-activity-indicator (&body body)
  "Turn on activity indicator, perform BODY and turn off activity indicator"
  `(progn
     (ztree.view.message::show-activity t)
     ,@body
     (ztree.view.message::show-activity nil)))

;;; message-view.lisp ends here
