;;; wm-ui.lisp --- Window (view) manager

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

(defpackage :zdircmp.ui.wm
  (:use :common-lisp
        :cl-ncurses
        :zdircmp.constants
        :zdircmp.ui.utils
        :zdircmp.view.mixin)
  (:shadowing-import-from :zdircmp.view.mixin :refresh)
  (:export :with-window-manager
           :get-window-manager
           :screen-width
           :screen-height
           :push-window
           ))

(in-package :zdircmp.ui.wm)

(defvar *global-window-manager* nil)

(define-condition on-bad-screen-size (error)
  ((message :initarg :description :reader description)))

(define-condition on-exit-command (error)
  ((text :initarg :text :reader text)))


(defclass window-manager ()
  ((windows :initform ()
            :accessor windows
            :documentation "List of windows in descending z-order, i.e. topmost - the first")
   (min-width :initarg :min-width
              :initform 79
              :reader min-width
              :documentation "Minimum window width. On resize less than this size will exit the app")
   (min-height :initarg :min-height
               :initform 24
               :reader min-height
               :documentation "Minimum window height. On resize less than this size will exit the app")))

(defmacro with-window-manager (min-width min-height &body body)
  "Macro to create a ncurses environment with window manager"
  (let ((maxcols-name (gensym))
        (maxrows-name (gensym))
        (win-name (gensym)))
  `(unwind-protect
        (progn
          (setf *global-window-manager* (make-instance 'window-manager
                                                       :min-width ,min-width
                                                       :min-height ,min-height))
          (initscr)
          (cl-ncurses:refresh)
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
          (let ((,maxcols-name 0)
                (,maxrows-name 0))
            (getmaxyx *stdscr* ,maxrows-name ,maxcols-name)
            (handler-case
                (progn
                  ;; verify the screen size
                  (ensure-screen-sizes-ok *global-window-manager*
                                          ,maxcols-name
                                          ,maxrows-name)
                  ,@body
                  (main-loop *global-window-manager*))
              (on-bad-screen-size (what) (format *error-output* (description what)))
              (on-exit-command (command) (message *message-view* "Exiting..."))))
          (dolist (,win-name (windows *global-window-manager*))
            (destroy ,win-name)))
     (progn
       (endwin)))))


(defun get-window-manager ()
  *global-window-manager*)

(defgeneric screen-width (wm)
  (:documentation "Returns the number of columns of the screen"))

(defmethod screen-width ((wm window-manager))
  (let ((maxcols 0)
        (maxrows 0))
    (getmaxyx *stdscr* maxrows maxcols)
    maxcols))
  
(defgeneric screen-height (wm)
  (:documentation "Returns the number of rows of the screen"))

(defmethod screen-height ((wm window-manager))
  (let ((maxcols 0)
        (maxrows 0))
    (getmaxyx *stdscr* maxrows maxcols)
    maxrows))


(defgeneric ensure-screen-sizes-ok (wm width height)
  (:documentation "Verifies if the screen size is up to the one specified in WM"))

(defmethod ensure-screen-sizes-ok ((wm window-manager) width height)
  (unless (and (>= width (min-width wm))
               (>= height (min-height wm)))
    (signal 'on-bad-screen-size :description
            (format nil
                    "Too small window size: ~ax~a, required size ~ax~a"
                    width height (min-width wm) (min-height wm)))))


;; (defun process-resize ()
;;   (let ((maxcols 0)
;;         (maxrows 0))
;;     (getmaxyx *stdscr* maxrows maxcols)
;;     (assert-screen-sizes-ok maxcols maxrows)
;;     (wclear *stdscr*)
;;     (wrefresh *stdscr*)
;;     (resize *message-view* 0 (1- maxrows) maxcols 1)
;;     (resize *status-view* 0 (- maxrows 2) maxcols 1)
;;     (let ((main-view-height (- maxrows 2))
;;           (main-view-y 0))
;;       (when (and *help-view* (visible *help-view*))
;;         (resize *help-view* 0 0 maxcols +help-window-height+)
;;         (setf main-view-height (- main-view-height +help-window-height+)
;;               main-view-y (+ main-view-y +help-window-height+)))
;;       ;; resize the main window
;;       (resize *main-view* 0 main-view-y maxcols main-view-height))))

(defgeneric process-resize (wm)
  (:documentation "Process resize event - resize all stretched windows in window stack"))

(defmethod process-resize ((wm window-manager))
  )


(defgeneric handle-key (wm key)
  (:documentation "Generic key handler"))

(defmethod handle-key ((wm window-manager) key)
  (format *error-output* "Views: ~a~%" (windows wm))
  (case key
    ;; handle resize
    (-1 (process-resize wm))
    ;; process key by topmost window
    (t (unless (null (windows wm))
         (format *error-output* "Topmost view: ~a~%" (car (windows wm)))
         (process-key (car (windows wm)) key)))))

  
          #|
  (cond ((eq key +KEY-ESC+)
         (signal 'on-exit-command :text "exit"))
        ((eq key +KEY-F1+)
         (toggle-help-view))

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

        ;; handle resize event
        ((eq key -1)
         (process-resize))
        ;; process others in main view
        (t (process-key *main-view* key))))
        |#


(defgeneric main-loop (wm)
  (:documentation "Run the main application loop. Note the app will never exit from this point")) 

(defmethod main-loop ((wm window-manager))
  (let ((key nil))
    (loop while (setf key (getch)) do
         (handle-key wm key))))


(defgeneric push-window (wm v)
  (:documentation "Push the view V into the window stack"))

(defmethod push-window ((wm window-manager) v)
  (format *error-output* "Push called~%")
  (push v (windows wm)))


;;; wm-ui.lisp ends here
