;;; base-view.lisp --- base class for TUI view (window) 

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

;; Base View

;;; Code:
(defpackage :zdircmp.view.base
  (:use ::common-lisp :cl-ncurses :zdircmp.ui.utils)
  ;; shadowing refresh from cl-ncurses, we don't use it anyway
  (:shadow :refresh)
  (:export :view
           :window
           :x
           :y
           :width
           :height
           :with-window
           :refresh
           :destroy
           :resize
           :visible
           :show
           :process-key
           :goto-point
           :text-out
           :vertical-line
           :horizontal-line
           :lower-left-corner))

(in-package :zdircmp.view.base)

(defmacro with-window ((v w) &body body)
  "When-let pattern. Set the W to the ncurses window and executes the body"
  `(let ((,w (window ,v)))
     (when ,w
       ,@body)))

(defstruct point
  "Point position. LINE is the line number in window, COLUMN is the column.
Both 0-based"
  (line 0)
  (column 0))

(defclass view ()
  ;; ncurses window
  ((window :initform nil :reader window)
   (window-rect :initarg :window-rect :reader window-rect)
   (client-rect :reader client-rect)
   (point :initform (make-point)
          :accessor point
          :documentation "Current point position"))
  (:documentation "Base class for ncurses-based views"))

(defgeneric x (v)
  (:documentation "X view position in global system"))
(defmethod x ((v view))
  (rect-x (window-rect v)))

(defgeneric y (v)
  (:documentation "Y view position in global system"))
(defmethod y ((v view))
  (rect-y (window-rect v)))

(defgeneric width (v)
  (:documentation "View width"))
(defmethod width ((v view))
  (rect-width (window-rect v)))

(defgeneric height (v)
  (:documentation "View hegiht"))
(defmethod height ((v view))
  (rect-height (window-rect v)))

;; constructor for the view
(defmethod initialize-instance :after ((v view) &rest args)
  ;; ignore unused args warning
  (declare (ignore args))
  (with-slots (window window-rect client-rect) v
    (setf window (newwin (height v) (width v) (y v) (x v)))
    (setf client-rect (copy-rect window-rect))
    (refresh v)
    (wrefresh (window v))))


(defgeneric goto-point (v &key line col)
  (:documentation "Move point to the position LINE,COL in client coordinate system"))
(defmethod goto-point ((v view) &key (line (point-line (point v)))
                                  (col (point-column (point v))))
  (setf (point-line (point v)) line)
  (setf (point-column (point v)) col))


(defgeneric text-out (v string &key with-color line col))
(defmethod text-out ((v view) string &key (with-color :white) line col)
  (with-window (v w)
    (let* ((l (if line line 
                  (point-line (point v))))
           (c (if col col (point-column (point v))))
           (endpos (+ c (length string))))
      (goto-point v :line l :col c )
      (with-color-win (w with-color)
        (mvwprintw w l c string))
      (goto-point v :col endpos)
      endpos)))

(defgeneric vertical-line (v x y length &key with-color)
  (:documentation "Draws the vertical line in current window starting from position
[X,Y] drawing LENGTH rows"))

(defmethod vertical-line ((v view) x y length &key (with-color :white))
  (with-window (v w)
    (with-color-win (w with-color)
      (mvwvline w
                y          ; y start position
                x          ; x position
                ACS_VLINE  ; character
                length)))) ; number of rows to draw

(defgeneric horizontal-line (v x y length &key with-color)
  (:documentation "Draws the horizontal line in current window starting from position
[X,Y] drawing LENGTH columns"))

(defmethod horizontal-line ((v view) x y length &key (with-color :white))
  (with-window (v w)
    (with-color-win (w with-color)
      (mvwhline w
                y          ; y start position
                x          ; x position
                ACS_HLINE  ; character
                length)))) ; number of columns to draw



(defgeneric current-point (v)
  (:documentation "Returns the tuple (cons line column) of the current point position"))
(defmethod current-point ((v view))
  (cons (point-line (point v))
        (point-column (point v))))

(defgeneric destroy (v)
  (:documentation "Destroy the associated with view ncurses window"))

(defmethod destroy ((v view))
  (with-window (v w)
    (with-slots (window) v
      (delwin w)
      (setf window nil))))

(defgeneric refresh (v &key force)
  (:documentation "Refreshes the associated ncurses window"))

(defmethod refresh :before ((v view) &key (force t))
  (declare (ignore force))
  (with-window (v w)
    (wclear w)))

(defmethod refresh :after ((v view) &key (force t))
  (declare (ignore force))
  (with-window (v w)
    (wrefresh w)))


(defgeneric resize (v x y width height)
  (:documentation "Process the resize command, resizing the associated ncurses window"))

(defmethod resize ((v view) x y width height)
  (with-window (v w)
    (with-slots (window-rect) v
      (setf window-rect
            (make-rect
             :x x
             :y y
             :width width
             :height height))
      (wclear w)
      (wresize w height width)
      (mvwin w y x)
      (refresh v :force t))))

(defgeneric visible (v)
  (:documentation "Determines if the window is visible"))

(defmethod visible ((v view))
  (window v))

(defgeneric show (v show)
  (:documentation "Show or hides window depending on SHOW argument"))

(defmethod show ((v view) show)
  (with-slots (window) v
    (with-window (v w)
      (wclear w)
      (wrefresh w)
      (delwin w))
    (setf window 
          (if show (newwin (height v)
                           (width v)
                           (y v)
                           (x v))
              nil))
    (refresh v)))

(defgeneric process-key (v key)
  (:documentation "Key handler for view"))


(defgeneric lower-left-corner (v x y &key with-color)
  (:documentation "Draw the sign of the lower-left corner of the box"))
(defmethod lower-left-corner ((v view) x y &key (with-color :white))
  (with-window (v w)
    (with-color-win (w with-color)
      (mvwaddch w y x ACS_LLCORNER))))
  

;;; base-view.lisp ends here
