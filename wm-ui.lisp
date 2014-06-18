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
           :quit-application
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
               :documentation "Minimum window height. On resize less than this size will exit the app")
   (screen-width :initform 0
                 :reader screen-width
                 :documentation "Current screen width. Updated in process-resize")
   (screen-height :initform 0
                  :reader screen-height
                  :documentation "Current screen height. Updated in process-resize")))
   

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
                  (update-screen-size *global-window-manager* ,maxcols-name ,maxrows-name)
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

(defgeneric update-screen-size (wm width height)
  (:documentation "Update current screen size to one specified"))

(defmethod update-screen-size ((wm window-manager) width height)
  (with-slots (screen-width screen-height) wm
    (setf screen-width width
          screen-height height)))
    

(defgeneric ensure-screen-sizes-ok (wm width height)
  (:documentation "Verifies if the screen size is up to the one specified in WM"))

(defmethod ensure-screen-sizes-ok ((wm window-manager) width height)
  (unless (and (>= width (min-width wm))
               (>= height (min-height wm)))
    (signal 'on-bad-screen-size :description
            (format nil
                    "Too small window size: ~ax~a, required size ~ax~a"
                    width height (min-width wm) (min-height wm)))))

(defgeneric process-resize (wm)
  (:documentation "Process resize event - resize all stretched windows in window stack"))

(defun calculate-new-size (pos size old-screen-size new-screen-size)
  "Calculate the new width/height aligning by left/bottom sides of the screen.
POS - either x or y
SIZE - either window width or height
OLD-SCREEN-SIZE - old screen either width or height
NEW-SCREEN-SIZE - new screen either width or height.
Calculation example:
width = 60;
x = 10;
oldScreenWidth = 70;
newScreenWidth = 60;
diff = newScreenWidth - (x + width)
newWidth = if x + width == oldScreenWidth
           then newScreenWidth - x
           else 
             if diff < 0
             then width + diff
             else width
gives us width 50"
  (let ((diff (- new-screen-size (+ pos size))))
    (cond ((= (+ pos size) old-screen-size)
           (- new-screen-size pos))
          ((< diff 0)
           (+ size diff))
          (t size))))
              
                           

(defmethod process-resize ((wm window-manager))
  (let ((new-screen-width 0)
        (new-screen-height 0))
    (getmaxyx *stdscr* new-screen-height new-screen-width)
    (ensure-screen-sizes-ok wm new-screen-width new-screen-height)
    (wclear *stdscr*)
    (wrefresh *stdscr*)
    (dolist (w (windows wm))
      (let* ((r (window-rect w))
             (new-width (calculate-new-size (rect-x r)
                                            (rect-width r)
                                            (screen-width wm)
                                            new-screen-width))
             (new-height (calculate-new-size (rect-y r)
                                             (rect-height r)
                                             (screen-height wm)
                                             new-screen-height)))
        ;; only resize visible views
        (when (visible w)
          (if (and (> new-width 0)
                   (> new-height 0))
              (resize w (rect-x r) (rect-y r) new-width new-height)
              (show w nil)))))
    (update-screen-size wm new-screen-width new-screen-height)))


(defgeneric handle-key (wm key)
  (:documentation "Generic key handler"))

(defmethod handle-key ((wm window-manager) key)
  (format *error-output* "handle-key: ~a~%" key)
  (alexandria:switch (key)
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
  (push v (windows wm)))

(defgeneric quit-application (wm)
  (:documentation "Force to shut down the application"))

(defmethod quit-application ((wm window-manager))
  (signal 'on-exit-command :text "exit"))


;;; wm-ui.lisp ends here
