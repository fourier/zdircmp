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
           :window-rect
           :client-rect
           :width
           :height
           :with-window
           :refresh
           :destroy
           :resize
           :update-client-rect
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
  ((window :initform nil :reader window
           :documentation "NCurses window")
   (window-rect :initarg :window-rect :reader window-rect
                :documentation "Non-client window rect in screen coordinate system")
   (client-rect :reader client-rect
                :documentation "Client window rect in window coordinate system")
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

(defgeneric update-client-rect (v)
  (:documentation "Updates the client rect after resize. Should be implemented
in classes which use not default client rect"))

;; constructor for the view
(defmethod initialize-instance :after ((v view) &rest args)
  ;; ignore unused args warning
  (declare (ignore args))
  (with-slots (window window-rect) v
    (setf window (newwin (height v) (width v) (y v) (x v)))
    (update-client-rect v)
    (refresh v)
    (wrefresh (window v))))

(defmethod update-client-rect ((v view))
  (with-slots (window-rect client-rect) v
    (setf client-rect (make-rect :width (rect-width window-rect)
                                 :height (rect-height window-rect)))))


(defgeneric goto-point (v &key line col)
  (:documentation "Move point to the position LINE,COL in client coordinate system"))
(defmethod goto-point ((v view) &key (line (point-line (point v)))
                                  (col (point-column (point v))))
  (setf (point-line (point v)) line)
  (setf (point-column (point v)) col))


(defgeneric text-out (v string &key with-color line col))
(defmethod text-out ((v view) string &key (with-color :white)
                                       (line (point-line (point v)))
                                       (col (point-column (point v))))
  (let ((client-l (rect-y (client-rect v)))
        (client-c (rect-x (client-rect v)))
        (client-w (rect-width (client-rect v)))
        (client-h (rect-height (client-rect v))))
    (with-window (v w)
      (let* ((l (+ client-l line))              ; from client to screen
             (c (+ client-c col))               ; from client to screen
             (len (length string))
             (endpos (+ c len)))
        ;(goto-point v :line line :col col )
        (when (and (>= l client-l)
                   (<  l (+ client-l client-h))
                   (>= endpos client-c)
                   (< c (+ client-c client-w)))
          (with-color-win (w with-color)
            (mvwprintw w l c string)))
        (goto-point v :line line :col endpos)
        endpos))))

(defgeneric vertical-line (v x y length &key with-color)
  (:documentation "Draws the vertical line in current window starting from position
[X,Y] drawing LENGTH rows"))

(defmethod vertical-line ((v view) x y length &key (with-color :white))
  (with-window (v w)
    (with-color-win (w with-color)
      (when (> length 0)                ; only positive direction
      (let* ((client-left (rect-x (client-rect v)))
             (client-top (rect-y (client-rect v)))
             (client-width (rect-width (client-rect v)))
             (client-height (rect-height (client-rect v)))
             (client-bottom (+ client-top client-height))
             (screen-x (+ x client-left)) ; client to screen
             (screen-y (+ y client-top))  ; client to screen
             (screen-y-end-pos (+ screen-y length)))
        ;; guard against being left/right of the client rect or below
        (when (and (>= x 0)
                   (<  x client-width)
                   (< y client-height))
          ;; cut on top
          (when (< screen-y client-top)
            (setf screen-y client-top))
          ;; cut on bottom
          (when (> screen-y-end-pos client-bottom)
            (setf screen-y-end-pos client-bottom))
          (mvwvline w
                    screen-y    ; y start position
                    screen-x    ; x position
                    ACS_VLINE   ; character
                    (- screen-y-end-pos screen-y)))))))) ; number of rows to draw
        
(defgeneric horizontal-line (v x y length &key with-color)
  (:documentation "Draws the horizontal line in current window starting from position
[X,Y] drawing LENGTH columns"))

(defmethod horizontal-line ((v view) x y length &key (with-color :white))
  (with-window (v w)
    (with-color-win (w with-color)
      (let* ((client-left (rect-x (client-rect v)))
             (client-top (rect-y (client-rect v)))
             (client-width (rect-width (client-rect v)))
             (client-height (rect-height (client-rect v)))
             (client-right (+ client-left client-width))
             (screen-x (+ x client-left)) ; client to screen
             (screen-y (+ y client-top))  ; client to screen
             (screen-x-end-pos (+ screen-x length)))
        ;; guard against being below/above of the client rect or to the right
        (when (and (>= y 0)
                   (<  y client-height)
                   (< x client-width))
          ;; cut on left side
          (when (< screen-x client-left)
            (setf screen-x client-left))
          ;; cut on right side
          (when (> screen-x-end-pos client-right)
            (setf screen-x-end-pos client-right))
          (mvwhline w
                    screen-y    ; y start position
                    screen-x    ; x position
                    ACS_HLINE   ; character
                    (- screen-x-end-pos screen-x))))))) ; number of columns to draw



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

(defmethod refresh :around ((v view) &key (force t))
  (declare (ignore force))
  (with-window (v w)
    (wclear w)
    (call-next-method)
    (wrefresh w)))

(defgeneric resize (v x y width height)
  (:documentation "Process the resize command, resizing the associated ncurses window"))

(defmethod resize ((v view) x y width height)
  (with-window (v w)
    (with-slots (window-rect client-rect) v
      (setf window-rect
            (make-rect
             :x x
             :y y
             :width width
             :height height))
      (update-client-rect v)
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
  (let* ((client-left (rect-x (client-rect v)))
         (client-top (rect-y (client-rect v)))
         (w (rect-width (client-rect v)))
         (h (rect-height (client-rect v)))
         (screen-x (+ x client-left)) ; client to screen
         (screen-y (+ y client-top))) ; client to screen
    (when (and (>= screen-x client-left)
               (>= screen-y client-top)
               (< screen-x (+ client-left w))
               (< screen-y (+ client-top h)))
      (with-window (v w)
        (with-color-win (w with-color)
          (mvwaddch w screen-y screen-x ACS_LLCORNER))))))


;;; base-view.lisp ends here
