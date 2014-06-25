;;; main.lisp --- Entry point to the application

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

;; Main entry point to the application, including Text User Interface

;;; Code:
(defpackage :zdircmp.main
  (:use ::common-lisp
        :cl-ncurses
        :zdircmp.constants
        :zdircmp.ui.utils
        :zdircmp.ui.wm
        :zdircmp.view.base
        :zdircmp.view.message
        :zdircmp.view.help
        :zdircmp.view.status
        :zdircmp.view.main
        :zdircmp.util)
  (:shadowing-import-from :zdircmp.view.base :refresh)
  (:export :main))

(in-package :zdircmp.main)

;; resolving conflict - the sb-ext already has the timeout function,
;; so shadowing importing one from the cl-ncurses package
(shadowing-import 'timeout)

(defconstant +min-screen-width+ 60
  "Minimum supported screen width")

(defconstant +min-screen-height+ 15
  "Minimum supported screen height")

(defconstant +help-window-height+ 9
  "Height of the help window")

(defvar *help-view* nil
  "Help view")

(defvar *message-view* nil
  "Messages view instance")

(defvar *status-view* nil
  "Status view")

(defvar *main-view* nil
  "Main application view")




(defun toggle-help-view ()
  (if (not *help-view*)
    (let ((maxcols 0)
          (maxrows 0))
      (getmaxyx *stdscr* maxrows maxcols)
      (setf *help-view* (make-help-view 0 0 maxcols +help-window-height+)))
    (show *help-view* (not (visible *help-view*))))
  (process-resize))


(defun command-line ()
  (or 
   #+SBCL sb-ext:*posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(defun usage (appname)
  (format *standard-output* (concat  "Usage: " appname " path1 path2~%"))
  (format *standard-output* "where path1 and path2 - paths to directories to compare~%")
  #+SBCL (sb-ext:exit)
  #+LISPWORKS (lispworks:quit)
  )

(defun main ()
  (let ((cmdargs (command-line)))
    (if (< (length cmdargs) 3)
        (usage (car cmdargs))
        (let ((left-path (second cmdargs))
              (right-path (third cmdargs)))
          (swank:create-server :port 4006)
          (with-window-manager +min-screen-width+ +min-screen-height+
              (let ((maxrows (screen-height (default-window-manager)))
                    (maxcols (screen-width (default-window-manager))))
                (setf *message-view* (make-message-view 0
                                                        (1- maxrows)
                                                        maxcols
                                                        1))
                ;; create a help window if necessary
                (let ((main-view-height (- maxrows 2))
                      (main-view-y 0))
                  (setf *status-view* (make-status-view 0
                                                        (- maxrows 2)
                                                        maxcols
                                                        1
                                                        left-path
                                                        right-path))
                  ;; create the main window
                  (setf *main-view* (make-main-view 0
                                                    main-view-y
                                                    maxcols
                                                    main-view-height))
                  ;; create a model node
                  (with-activity-indicator (*message-view*)
                    (set-model-node *main-view*
                                    (zdircmp.model.node::create-root-node
                                     left-path
                                     right-path
                                     :message-observer  *message-view*
                                     :activity-observer *message-view*)))
                  (message *message-view* "Press F1 for quick help, ESC to exit"))))))))


;;; main.lisp ends here
