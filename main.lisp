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


(define-condition on-bad-screen-size (error)
  ((message :initarg :description :reader description)))

(define-condition on-exit-command (error)
  ((text :initarg :text :reader text)))

(defun assert-screen-sizes-ok (width height)
  (unless (and (>= width +min-screen-width+)
               (>= height +min-screen-height+))
    (signal 'on-bad-screen-size :description
            (format nil
                    "Too small window size: ~ax~a, required size ~ax~a"
                    width height +min-screen-width+ +min-screen-height+))))


(defun process-resize ()
  (let ((maxcols 0)
        (maxrows 0))
    (getmaxyx *stdscr* maxrows maxcols)
    (assert-screen-sizes-ok maxcols maxrows)
    (wclear *stdscr*)
    (wrefresh *stdscr*)
    (resize *message-view* 0 (1- maxrows) maxcols 1)
    (resize *status-view* 0 (- maxrows 2) maxcols 1)
    (let ((main-view-height (- maxrows 2))
          (main-view-y 0))
      (when (visible *help-view*)
        (resize *help-view* 0 0 maxcols +help-window-height+)
        (setf main-view-height (- main-view-height +help-window-height+)
              main-view-y (+ main-view-y +help-window-height+)))
      ;; resize the main window
      (resize *main-view* 0 main-view-y maxcols main-view-height))))


(defun toggle-help-view ()
  (if (not *help-view*)
    (let ((maxcols 0)
          (maxrows 0))
      (getmaxyx *stdscr* maxrows maxcols)
      (setf *help-view* (make-help-view 0 0 maxcols +help-window-height+)))
    (show *help-view* (not (visible *help-view*))))
  (process-resize))


(defun handle-key (key)
  ;; handle F1-F12 in main app
  (cond ((eq key +KEY-ESC+)
         (signal 'on-exit-command :text "exit"))
        ((eq key +KEY-F1+)
         (toggle-help-view))
        #|
        ((eq key +KEY-F2+) 
         (message *message-view* "F2"))
        ((eq key +KEY-F3+) 
         (message *message-view* "F3"))
        ((eq key +KEY-F4+) 
         (message *message-view* "F4"))
        ((eq key +KEY-F5+) 
         (message *message-view* "F5"))
        ((eq key +KEY-F6+) 
         (message *message-view* "F6"))
        ((eq key +KEY-F7+) 
         (message *message-view* "F7"))
        ((eq key +KEY-F8+) 
         (message *message-view* "F8"))
        ((eq key +KEY-F9+) 
         (message *message-view* "F9"))
        ((eq key +KEY-F10+) 
         (message *message-view* "F10"))
        ((eq key +KEY-F11+) 
         (message *message-view* "F11"))
        ((eq key +KEY-F12+) 
         (message *message-view* "F12"))
        |#
        ;; handle resize event
        ((eq key -1)
         (process-resize))
        ;; process others in main view
        (t (process-key *main-view* key))))

(defun command-line ()
  (or 
   #+SBCL sb-ext:*posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(defun usage (appname)
  (format *error-output* (concat  "Usage: " appname " path1 path2~%"))
  (format *error-output* "where path1 and path2 - paths to directories to compare~%")
  #+SBCL (sb-ext:exit)
  #+LISPWORKS (lispworks:quit)
  )

  

(defun main ()
  (let ((cmdargs (command-line)))
    (if (< (length cmdargs) 3)
        (usage (car cmdargs))
        (let ((left-path (second cmdargs))
              (right-path (third cmdargs)))
          ;(swank:create-server :port 4006)
          (with-ncurses
              ;; no caching of the input
              (cbreak)
            ;; turn on special keys 
            (keypad *stdscr* 1)
            ;; turn on color 
            (start-color)
            ;; initialize color pairs
            (init-color-pairs)
            ;; turn off key echoing
            (noecho)
            ;; hide cursor
            (curs-set 0)
            ;; clear and refresh screen
            (clear)
            ;; refresh is shadowed
            (cl-ncurses:refresh)
            ;; get the screen dimensions
            (let ((maxcols 0)
                  (maxrows 0))
              (getmaxyx *stdscr* maxrows maxcols)
              (handler-case
                  (progn
                    ;; verify the screen size
                    (assert-screen-sizes-ok maxcols maxrows)
                    ;; create the messages window
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
                                         :message-function (curry #'message *message-view*)
                                         :activity-function (curry #'update-activity *message-view*))))
                      (message *message-view* "Press F1 for quick help, ESC to exit")
                      ;; keyboard input loop with ESC as an exit condition
                      (let ((key nil))
                        (loop while (setf key (getch)) do
                             (handle-key key)))))

                ;; error handling: wrong screen size
                (on-bad-screen-size (what) (format *error-output* (description what)))
                (on-exit-command (command) (message *message-view* "Exiting..."))))
            ;; destroy windows
            (destroy *help-view*)
            (destroy *message-view*)
            (destroy *main-view*))))))


;;; main.lisp ends here
