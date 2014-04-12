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
(defpackage :ztree.main
  (:use ::common-lisp :cl-ncurses :ztree.constants :ztree.ui.utils)
  ;; import message function from ztree.view.message for easy use of messages
  (:import-from :ztree.view.message :message)
  (:export :main))

(in-package :ztree.main)

;; resolving conflict - the sb-ext already has the timeout function,
;; so shadowing importing one from the cl-ncurses package
(shadowing-import 'timeout)

(defconstant +min-screen-width+ 60
  "Minimum supported screen width")

(defconstant +min-screen-height+ 15
  "Minimum supported screen height")

(defconstant +help-window-height+ 5
  "Height of the help window")

(defvar *help-window-visible* t)


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
    (ztree.view.message:resize-view 0 (1- maxrows) maxcols 1)
    (let ((main-view-height (1- maxrows))
          (main-view-y 0))
      (when *help-window-visible*
        (ztree.view.help:resize-view 0 0 maxcols +help-window-height+)
        (setf main-view-height (- main-view-height +help-window-height+))
        (setf main-view-y (+ main-view-y +help-window-height+)))
      ;; create the main window
      (ztree.view.main:resize-view 0 main-view-y maxcols main-view-height))))


(defun toggle-help-view ()
  (setf *help-window-visible* (not *help-window-visible*))
  (ztree.view.help:show-view *help-window-visible*)
  (process-resize)
  (message (format nil "~a help window"
                   (if *help-window-visible* "Showing" "Hiding"))))


(defun handle-key (key)
  ;; handle F1-F12 in main app
  (cond ((eq key +KEY-ESC+)
         (signal 'on-exit-command :text "exit"))
        ((eq key +KEY-F1+)
         (toggle-help-view))
        ((eq key +KEY-F2+) 
         (message "F2"))
        ((eq key +KEY-F3+) 
         (message "F3"))
        ((eq key +KEY-F4+) 
         (message "F4"))
        ((eq key +KEY-F5+) 
         (message "F5"))
        ((eq key +KEY-F6+) 
         (message "F6"))
        ((eq key +KEY-F7+) 
         (message "F7"))
        ((eq key +KEY-F8+) 
         (message "F8"))
        ((eq key +KEY-F9+) 
         (message "F9"))
        ((eq key +KEY-F10+) 
         (message "F10"))
        ((eq key +KEY-F11+) 
         (message "F11"))
        ((eq key +KEY-F12+) 
         (message "F12"))
        ;; handle resize event
        ((eq key -1)
         (process-resize))
        ;; process others in main view
        (t (ztree.view.main:process-key key))))

(defun command-line ()
  (or 
   #+SBCL sb-ext:*posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(defun usage (appname)
  (format *error-output* (concatenate 'string "Usage: " appname " path1 path2~%"))
  (format *error-output* "where path1 and path2 - paths to directories to compare~%")
  (sb-ext:quit))


(defun main ()
  (let ((cmdargs (command-line)))
    (if (< (length cmdargs) 3)
        (usage (car cmdargs))
        (let ((left-path (second cmdargs))
              (right-path (third cmdargs)))
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
            (refresh)
            ;; get the screen dimensions
            (let ((maxcols 0)
                  (maxrows 0))
              (getmaxyx *stdscr* maxrows maxcols)
              (handler-case
                  (progn
                    ;; verify the screen size
                    (assert-screen-sizes-ok maxcols maxrows)
                    ;; create the messages window
                    (ztree.view.message:create-view 0 (1- *lines*) *cols* 1)
                    ;; create a help window if necessary
                    (let ((main-view-height (1- *lines*))
                          (main-view-y 0))
                      (when *help-window-visible*
                        (ztree.view.help:create-view 0 0
                                                     *cols* +help-window-height+
                                                     left-path
                                                     right-path)
                        (setf main-view-height (- main-view-height +help-window-height+))
                        (setf main-view-y (+ main-view-y +help-window-height+)))
                      ;; create the main window
                      (ztree.view.main:create-view 0 main-view-y *cols* main-view-height))
                    ;; create a model node
                    (ztree.view.main:set-model-node 
                     (ztree.model.node::create-root-node left-path right-path :message-function 'message))
                    ;; keyboard input loop with ESC as an exit condition
                    (let ((key nil))
                      (loop while (setf key (getch)) do
                           (handle-key key))))

                ;; error handling: wrong screen size
                (on-bad-screen-size (what) (format *error-output* (description what)))
                (on-exit-command (command) (message "Exiting..."))))
            ;; destroy windows
            (ztree.view.help:destroy-view)
            (ztree.view.message:destroy-view)
            (ztree.view.main:destroy-view))))))

