;;; main-view.lisp --- main TUI view (window) for directory trees

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

;; Main View

;;; Code:
(defpackage :zdircmp.view.main
  (:use ::common-lisp :cl-ncurses
        :zdircmp.util
        :zdircmp.model.node
        :zdircmp.model.tree
        :zdircmp.constants
        :zdircmp.view.base
        :zdircmp.ui.utils
        :zdircmp.ui.command)
  ;; shadowing refresh from cl-ncurses, we use the one in base-view
  (:shadowing-import-from :zdircmp.view.base :refresh)
  (:export :make-main-view
           :process-key
           :set-model-node))

(in-package :zdircmp.view.main)

(defconstant +left-offset+ 2)


(defstruct cursor
  "Cursor position. LINE is the line number in window, SIDE
is the side of the cursor - 'zdircmp.model.node::left or 'zdircmp.model.node::right"
  (line 0)
  (side 'zdircmp.model.node::left))


(defclass main-view (view)
  ((node :initarg node
         :initform nil
         :accessor node
         :documentation "Root node")
   (start-line :initarg start-line
               :initform 0
               :accessor start-line
               :documentation "Line number from the tree where the screen starts")
   (cursor :initform (make-cursor)
           :accessor cursor
           :documentation "Cursor determines position of the visible cursor")
   (last-command :initform (make-instance 'user-command)
                 :accessor last-command
                 :documentation "Last entered user command"))
  (:documentation "Main application view with directory trees and separator"))


(defmacro defcommand ((v name) &body body)
  "Declares the command function. Usage example:
`(defcommand (v up) (let ((a 1)) a))
generates the function
`(DEFMETHOD PROCESS-UP ((V MAIN-VIEW))
  (LET ((A 1))
    A)
  (SETF (LAST-COMMAND V) 'UP))"
  (let ((command-fun-name (intern
                           (string-upcase
                            (concatenate 'string "process-" (symbol-name name))))))
    `(progn
       (defgeneric ,command-fun-name (,v))
       (defmethod ,command-fun-name ((,v main-view))
         ,@body
         (setf (last-command ,v) (quote ,name) )))))

(defun make-main-view (x y width height)
  (make-instance 'main-view
                 :x x
                 :y y
                 :width width
                 :height height))


(defun message (&rest args)
  (apply #'format *ERROR-OUTPUT* args)
  (format *ERROR-OUTPUT* "~%"))

#|
(defmethod destroy :before ((v view))
  "Clears and destroys the ncurses window"
  (with-window (v w)
    ;; border with spaces
    (wborder w 32 32 32 32 32 32 32 32)))
|#

(defmethod refresh ((v main-view) &key (force t))
  ;; if we have a ncurses window
  (with-window (v w)
                                        ;(format *ERROR-OUTPUT* "Refresh called: ~a :force ~a ~%" (class-name (class-of v)) force)
    (box w 0 0)
    (when (node v)
      (refresh-contents v force))))


(defgeneric window-line-to-tree-line (v            window-line))
(defmethod window-line-to-tree-line ((v main-view) window-line)
  "Returns the line in the tree array by given WINDOW-LINE 0-based line in window."
  (+ (start-line v) window-line))

(defgeneric tree-line-to-window-line (v            line))
(defmethod tree-line-to-window-line ((v main-view) line)
  "Returns line in window by given tree array line.
May be negative or more than window size"
  (- line (start-line v)))

(defgeneric line-number-at-pos (v))
(defmethod line-number-at-pos ((v main-view))
  (let* ((cursor-pos (cursor-line (cursor v)))
         (line (window-line-to-tree-line v cursor-pos)))
    line))

(defgeneric line-visible-p (v            line))
(defmethod line-visible-p ((v main-view) line)
  "Returns t if the tree-line is visible in the current screen"
  (let ((window-line (tree-line-to-window-line v line))
        (max-line (1- (height v))))
    (and (>= window-line 0)
         (< window-line max-line))))

(defun get-new-position (number-of-lines start-line height tree-size)
  "Calculate the new start screen line by given NUMBER-OF-LINES to scroll,
which may be negative, START-LINE - line number of the most upper visible line,
HEIGHT - the window height in lines and TREE-SIZE - the number of lines
in the buffer"
  (let* ((desired-new-position (+ start-line number-of-lines))
         (allowed-new-position (- tree-size height))
         (new-position (min desired-new-position allowed-new-position)))
    (if (>= new-position 0) new-position 0)))

(defgeneric scroll-lines (v            number-of-lines))
(defmethod scroll-lines ((v main-view) number-of-lines)
  "Scrolls the current window to NUMBER-OF-LINES"
  (let ((new-position (get-new-position number-of-lines
                                        (start-line v)
                                        (- (height v) 2)
                                        (tree-number-of-lines))))
    (setf (start-line v) new-position)
    (refresh v :force nil)))

(defgeneric scroll-to-line (v            line))
(defmethod scroll-to-line ((v main-view) line)
  "Set the cursor to the LINE, line is the line in tree(buffer),
not a screen line. If the line is visible (on screen), just set cursor to
this line, otherwise:
1. If the line is above the screen, set the line as a first line in
the screen
2. If the line is below the screen, set is as high as possible"
  ;; if line is visible set the cursor to this line
  (if (line-visible-p v line)
      (setf (cursor-line (cursor v))
            (tree-line-to-window-line v line))
      ;; otherwise scroll to the number of lines - works only
      ;; for scroll up for now. TODO: fix it for scroll down - case 2
      (let ((window-line (tree-line-to-window-line v line)))
        (scroll-lines v window-line)
        (setf window-line (tree-line-to-window-line v line))
        (setf (cursor-line (cursor v))
              (tree-line-to-window-line v line))))
  (refresh v :force nil))

;; UP key event handler - move cursor up one line
(defcommand (v up)
  (let ((cursor-pos (cursor-line (cursor v))))
    ;; if the cursor is not on the first line, just move it up
    (if (> cursor-pos 0)
        (progn
          (setf (cursor-line (cursor v)) (1- cursor-pos))
          (refresh v :force nil))
        ;; otherwise - cursor is on the same place, but window moves up
        (scroll-lines v -1))))

;; DOWN key event handler - move cursor down one line
(defcommand (v down)
  (let ((cursor-pos (cursor-line (cursor v))))
    (when (/= (+ (start-line v) cursor-pos)
              (1- (tree-number-of-lines)))
      ;; if the cursor is on the last line, scroll down, cursor is on the same place
      (if (= cursor-pos (- (height v) 3))
          (scroll-lines v 1)
          (progn
            ;; otherwise update its position
            (setf (cursor-line (cursor v)) (1+ cursor-pos))
            (refresh v :force nil))))))

;; PAGE UP key event handler - scrolls up 1 screen if possible
(defcommand (v pgup)
  (scroll-lines v (- (height v))))

;; PAGE DOWN key event handler - scrolls down 1 screen if possible"
(defcommand (v pgdn)
  (scroll-lines v (height v)))

;; ENTER key event handler - opens/close directories"
(defcommand (v return)
  (let* ((line (line-number-at-pos v))
         (node (tree-entry-node (tree-entry-at-line line)))
         (expanded (node-expanded-p node))
         (node-is-file (not (diff-node-is-directory node))))
    (if (and node-is-file
             (eq (diff-node-side node) 'zdircmp.model.node::both))
        (let ((left (diff-node-left-path node))
              (right (diff-node-right-path node)))
          (def-prog-mode)
          (endwin)
          #+asdf3
          (asdf/run-program:run-program (list "vimdiff" left right) :ignore-error-status t :output :interactive)
          #-asdf3
          (asdf:run-shell-command (format nil "vimdiff \"~a\" \"~a\"" left right))
          (reset-prog-mode)
          (cl-ncurses:refresh))
        (toggle-expand-state-by-line line (not expanded))))
  (refresh v))

;; TAB key event handler - jump to the other side of the window"
(defcommand (v tab)
  (let ((side (cursor-side (cursor v))))
    (if (eq side 'zdircmp.model.node::left)
        (setf (cursor-side (cursor v))
              'zdircmp.model.node::right)
        (setf (cursor-side (cursor v))
              'zdircmp.model.node::left)))
  (refresh v :force nil))

;; Action on Backspace key: to jump to the line of a parent node or
;; if the node is directory and expanded - close it
(defcommand (v backspace)
  (let* ((line (line-number-at-pos v))
         (entry (tree-entry-at-line line))
         (parent-line (tree-entry-parent-line entry))
         (is-dir (diff-node-is-directory (tree-entry-node entry)))
         (expanded (node-expanded-p (tree-entry-node entry))))
    ;; perform action on any node except the root node
    (when (/= parent-line line)
      (if (and is-dir
               expanded)
          (progn
            (toggle-expand-state-by-line line nil)
            (refresh v))
          (scroll-to-line v parent-line)))))

;; left key event handler - jump to left pane if cursor is on the right
(defcommand (v left)
  (let ((side (cursor-side (cursor v))))
    (when (eq side 'zdircmp.model.node::right)
      (setf (cursor-side (cursor v))
            'zdircmp.model.node::left)
      (refresh v :force nil))))

;; right key event handler - jump to right pane if cursor is on the left
(defcommand (v right)
  (let ((side (cursor-side (cursor v))))
    (when (eq side 'zdircmp.model.node::left)
      (setf (cursor-side (cursor v))
            'zdircmp.model.node::right)
      (refresh v :force nil))))


(defmethod process-key ((v main-view) key)
  "Keypress dispatcher"
  (cond ((eq key +KEY-UP+)
         (process-up v))
        ((eq key +KEY-DOWN+)
         (process-down v))
        ((eq key +KEY-LEFT+) 
         (process-left v))
        ((eq key +KEY-RIGHT+) 
         (process-right v))
        ((eq key +KEY-ENTER+) 
         (process-return v))
        ((eq key +KEY-SPACE+) 
         (message "SPACE"))
        ((key-backspace-p key)
         (process-backspace v))
        ((eq key +KEY-HOME+) 
         (message "HOME"))
        ((eq key +KEY-END+) 
         (message "END"))
        ((eq key +KEY-TAB+) 
         (process-tab v))
        ((eq key +KEY-NPAGE+) 
         (process-pgdn v))
        ((eq key +KEY-PPAGE+)
         (process-pgup v))
        ((eq key +KEY-F1+) 
         (message "F1"))
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
        (t
         (message "Pressed: ~a" key))))

(defgeneric set-model-node  (v            nod))
(defmethod  set-model-node ((v main-view) nod)
  "Sets the current root node of the model. Refreshes the window
and redraws all data inside"
  (setf (node v) nod)
  (refresh-tree nod)
  (refresh v))

(defgeneric print-entry-in-window (v            line))
(defmethod print-entry-in-window ((v main-view) line)
  (let* ((entry (tree-entry-at-line line))
         (node (tree-entry-node entry))
         (offset (tree-entry-offset entry))
         (side (diff-node-side node))
         (short-name (if (or (eq side 'zdircmp.model.node::both)
                             (eq side 'zdircmp.model.node::left))
                         (diff-node-short-name node)
                         (diff-node-right-short-name node)))
         (expandable (diff-node-is-directory node))
         (expanded   (node-expanded-p node))
         (diff       (diff-node-different node))
         (parent-line (tree-entry-parent-line entry)))
    (if (eq side 'zdircmp.model.node::both)
        ;; insert left AND right labels
        (progn 
          (insert-single-entry v
                               short-name
                               expandable
                               expanded
                               line
                               parent-line
                               offset
                               'zdircmp.model.node::left
                               diff)
          (insert-single-entry v
                               short-name
                               expandable
                               expanded
                               line
                               parent-line
                               offset
                               'zdircmp.model.node::right
                               diff))
        (progn
          (insert-single-entry v
                               short-name
                               expandable
                               expanded
                               line
                               parent-line
                               offset
                               side
                               diff)
          (insert-dummy-cursor-opposite-side v
                                             (1+ (tree-line-to-window-line v line))
                                             offset
                                             side)))))

(defgeneric insert-dummy-cursor-opposite-side (v            window-line offset side))
(defmethod insert-dummy-cursor-opposite-side ((v main-view) window-line offset side)
  (let* ((middle (floor (/ (- (width v) 2) 2)))
         (x-position (+ +left-offset+
                        (* offset 4)
                        (if (eq side 'zdircmp.model.node::left) middle 0))))
    (when (and (= (1- window-line)
                  (cursor-line (cursor v)))
               t)
      (text-out v " " :col x-position :line window-line :with-color :black-on-white))))



(defun color-for-diff (diff)
  (cond ((eq diff 'zdircmp.model.node::diff) :red)
        ((eq diff 'zdircmp.model.node::new)  :cyan)
        (t :white)))

(defun inverse-color (color)
  (cond ((eq color :red) :red-on-white)
        ((eq color :blue) :blue-on-white)
        ((eq color :green) :green-on-white)
        (t :black-on-white)))

(defgeneric insert-single-entry (v
                                 short-name
                                 expandable expanded
                                 line
                                 parent-line
                                 offset
                                 side
                                 diff))
(defmethod insert-single-entry ((v main-view)
                                short-name
                                expandable expanded
                                line
                                parent-line
                                offset
                                side
                                diff)
  ;; if the node is expandable the [+] shall be inserted before
  ;; and the text position shifted by 4:
  ;; [+] some text 
  ;; ^   ^
  ;; 0   4
  ;; Also the horizontal line is shorter
  ;; horizontal line goes from the previous offset - 4 characters
  (let* ((middle (floor (/ (- (width v) 2) 2)))
         ;; x-position is the TEXT position. So the [+] sign position or tree leaf width
         ;; shall be calculated by subtracting from the x-position
         (x-position (+ +left-offset+     ; 1) offset from window left side (to move out of border)
                        (* (1+ offset) 4) ; 2) 4 chars for possible [+], and 4 chars per offset
                                        ; 3) shift to the middle for the right-side nodes
                        (if (eq side 'zdircmp.model.node::right) (1+ middle) 0))) 
         (height (height v))
         (window-line (1+ (tree-line-to-window-line v line)))
         (parent-window-line (tree-line-to-window-line v parent-line))
         ;; x-position is a text position, so in order to draw the line and
         ;; write a [+] sign we need to shift it to the left, but don't forget
         ;; the offset from the border
         (line-start (- x-position 4 +left-offset+))
         (line-length 5))
    (flet ((node-sign (exp x y)
             (let ((text (format nil "[~a]" (if exp "-" "+"))))
               (text-out v text :line y :col x :with-color :white))))
      (when expandable
        ;; horizontal line is shorter for directories by the [+] string
        (setq line-length (- line-length 3))
        ;; for expandable nodes insert "[+/-]"
        ;; guard against out-of window
        (when (and (> window-line 0)            ; don't draw on top border
                   (< window-line (1- height))) ; don't draw on bottom border
          (node-sign expanded (- x-position 4) window-line)))
      ;; if there is a space to draw
      (when (> offset 0)
        ;; draw horizontal line
        ;; guard against out-of window
        (when (and (> window-line 0)            ; don't draw on top border
                   (< window-line (1- height))) ; don't draw on bottom border
          (horizontal-line v line-start window-line line-length))
        ;; draw vertical line
        ;; first draw the ` character in the bottom of vertical line
        ;; any other nodes below will overwrite it with "|" anyways
        (when (and (> window-line 0)            ; don't draw on top border
                   (< window-line (1- height))) ; don't draw on bottom border
          (lower-left-corner v (1- line-start) window-line))
        ;; then draw a line, starting from the parent line, but don't forget
        ;; 1) the parent line index is 0-based (and it we need to shift it by 1
        ;; because of the border), and 2) we draw the line not from the parent
        ;; line, but from the line below the parent
        (let* ((start-line (if (< parent-window-line 0) 1 (+ 2 parent-window-line)))
               ;; we draw from parent to current excluding parent and current
               ;; lines
               (len (- window-line start-line))
               (end-line (+ start-line len)))
          (when (> end-line (- height 1))
            (setf len (- len (- end-line height) 1)))
          (vertical-line v
                         (1- line-start)   ; x start position
                         start-line        ; y position
                         len))))           ; number of rows to draw
    ;; determine if the line is under the cursor
    (let ((color (color-for-diff diff)))
      (when (and (= (1- window-line)
                    (cursor-line (cursor v)))
                 (eq (cursor-side (cursor v)) side))
        (setf color (inverse-color color)))
      ;; guard against out-of window
      (when (and (> window-line 0)            ; don't draw on top border
                 (< window-line (1- height))) ; don't draw on bottom border
        (text-out v short-name :line window-line :col x-position :with-color color)))))


(defgeneric refresh-contents (v do-refresh-model))
(defmethod refresh-contents ((v main-view) do-refresh-model)
  "Redraws all window's contents - tree(if DO-REFRESH-MODEL is t) and separator"
  ;; refresh tree in memory
  (when do-refresh-model
    (refresh-tree (node v)))
  ;; draw vertical separator
  (let* ((middle (floor (/ (- (width v) 2) 2))))
    (vertical-line v
                   (1+ middle)
                   1
                   (- (height v) 2))
    ;; draw all visible lines
    (dotimes (line (tree-number-of-lines))
      (print-entry-in-window v line))))


;;; main-view.lisp ends here
