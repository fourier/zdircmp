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
(defpackage :ztree.view.main
  (:use ::common-lisp :cl-ncurses :ztree.util :ztree.model.node :ztree.model.tree :ztree.constants)
  ;; import message function from ztree.view.message for easy use of messages
  (:import-from :ztree.view.message :message)
  (:export :create-view
           :destroy-view
           :resize-view
           :process-key
           :set-model-node
           :refresh-view))

(in-package :ztree.view.main)

(shadowing-import 'timeout)

(defconstant +left-offset+ 2)


(defstruct cursor
  "Cursor position. LINE is the line number in window, SIDE
is the side of the cursor - 'left or 'right"
  (line 0)
  (side 'left))

(defstruct main-window
  (window nil)
  (x 0)
  (y 0)
  (width 0)
  (height 0)
  (node nil)
  (start-line 0)
  (cursor (make-cursor)))


(defvar *main-window* (make-main-window)
  "Main ncurses window")

(defun destroy-view ()
  "Clears and destroys the ncurses window"
  (when (main-window-window *main-window*)
    ;; border with spaces
    (wborder (main-window-window *main-window*) 32 32 32 32 32 32 32 32)
    (wrefresh (main-window-window *main-window*))
    (delwin (main-window-window *main-window*))
    (setf (main-window-window *main-window*) nil)))

(defun create-view (x y width height)
  "Creates the ncurses window and refreshes its contents"
  (destroy-view)
  (setf (main-window-window *main-window*) (newwin height width y x))
  (setf (main-window-x *main-window*) x)
  (setf (main-window-y *main-window*) y)
  (setf (main-window-width *main-window*) width)
  (setf (main-window-height *main-window*) height)
  (refresh-view))


(defun refresh-view ()
  ;; if we have a ncurses window
  (when (main-window-window *main-window*)
    (wclear (main-window-window *main-window*))
    (box (main-window-window *main-window*) 0 0)
    (wrefresh (main-window-window *main-window*))
    (when (main-window-node *main-window*)
      (refresh-contents))))

(defun resize-view (x y width height)
  (wclear (main-window-window *main-window*))
  (wresize (main-window-window *main-window*) height width)
  (mvwin (main-window-window *main-window*) y x)
  (setf (main-window-x *main-window*) x)
  (setf (main-window-y *main-window*) y)
  (setf (main-window-width *main-window*) width)
  (setf (main-window-height *main-window*) height)
  (box (main-window-window *main-window*) 0 0)
  (refresh-view))


(defun get-new-position (number-of-lines start-line height tree-size)
  "Calculate the new start screen line by given NUMBER-OF-LINES to scroll,
which may be negative, START-LINE - line number of the most upper visible line,
HEIGHT - the window height in lines and TREE-SIZE - the number of lines
in the buffer"
  (let* ((desired-new-position (+ start-line number-of-lines))
         (allowed-new-position (- tree-size height))
         (new-position (min desired-new-position allowed-new-position)))
    (if (>= new-position 0) new-position 0)))

(defun scroll-lines (number-of-lines)
  "Scrolls the current window to NUMBER-OF-LINES"
  (let ((new-position (get-new-position number-of-lines
                                        (main-window-start-line *main-window*)
                                        (- (main-window-height *main-window*) 2)
                                        (tree-number-of-lines))))
    (setf (main-window-start-line *main-window*) new-position)
    (refresh-view)))

(defun process-key-up ()
  "UP key event handler - scrolls up 1 line contents if possible"
  (scroll-lines -1))

(defun process-key-down ()
  "DOWN key event handler - scrolls down 1 line contents if possible"
  (scroll-lines 1))

(defun process-key-pgup ()
  "PAGE UP key event handler - scrolls up 1 screen if possible"
  (scroll-lines (- (main-window-height *main-window*))))

(defun process-key-pgdn ()
  "PAGE DOWN key event handler - scrolls down 1 screen if possible"
  (scroll-lines (main-window-height *main-window*)))

(defun process-key-return ()
  "ENTER key event handler - opens/close directories"
  (let ((expanded (node-expanded-p (tree-entry-node (tree-entry-at-line 4)))))
    (toggle-expand-state-by-line 4 (not expanded))))

(defun process-key (key)
  "Keypress dispatcher"
  (cond ((eq key +KEY-UP+)
         (process-key-up))
        ((eq key +KEY-DOWN+)
         (process-key-down))
        ((eq key +KEY-LEFT+) 
         (message "LEFT"))
        ((eq key +KEY-RIGHT+) 
         (message "RIGHT"))
        ((eq key +KEY-ENTER+) 
         (process-key-return))
        ((eq key +KEY-SPACE+) 
         (message "SPACE"))
        ((key-backspace-p key)
         (message "BACKSPACE"))
        ((eq key +KEY-HOME+) 
         (message "HOME"))
        ((eq key +KEY-END+) 
         (message "END"))
        ((eq key +KEY-TAB+) 
         (message "TAB"))
        ((eq key +KEY-NPAGE+) 
         (process-key-pgdn))
        ((eq key +KEY-PPAGE+)
         (process-key-pgup))
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
         (message (format nil "Pressed: ~a" key))))
  (refresh-view))

(defun set-model-node (node)
  "Sets the current root node of the model. Refreshes the window
and redraws all data inside"
  (setf (main-window-node *main-window*) node)
  (refresh-tree node)
  (refresh-view))


(defun print-entry-in-window (entry window-line)
  (let* ((node (tree-entry-node entry))
         (offset (tree-entry-offset entry))
         (side (diff-node-side node))
         (short-name (if (or (eq side 'ztree.model.node::both)
                             (eq side 'ztree.model.node::left))
                         (diff-node-short-name node)
                         (diff-node-right-short-name node)))
         (expandable (diff-node-is-directory node))
         (expanded   (node-expanded-p node)))
    (if (eq side 'ztree.model.node::both)
        ;; insert left AND right labels
        (progn 
          (insert-single-entry short-name
                               expandable
                               expanded
                               window-line
                               offset
                               'ztree.model.node::left)
          (insert-single-entry short-name
                               expandable
                               expanded
                               window-line
                               offset
                               'ztree.model.node::right))
        (insert-single-entry short-name
                             expandable
                             expanded
                             window-line
                             offset
                             side))))


        
    ;; (mvwprintw (main-window-window *main-window*)
    ;;            window-line
    ;;            (+ +left-offset+ offset)
    ;;        (diff-node-short-name node))))


(defun insert-single-entry (short-name
                            expandable expanded
                            window-line
                            offset
                            side)
  (let* ((middle (floor (/ (- (main-window-width *main-window*) 2) 2)))
         (x-position (+ +left-offset+
                        (* offset 4)
                        (if (eq side 'ztree.model.node::right) middle 0)))
         (win (main-window-window *main-window*)))
    (flet ((node-sign (exp x y)
             (let ((text (format nil "[~a]" (if exp "-" "+"))))
               (mvwprintw win y x text))))
      (when expandable
        (node-sign expanded x-position window-line)   ; for expandable nodes insert "[+/-]"
        (setf x-position (+ x-position 4)))
      (mvwprintw win window-line x-position short-name))))

(defun refresh-contents ()
  "Redraws all window's contents - tree and separator"
  ;; refresh tree in memory
  (refresh-tree (main-window-node *main-window*))
  ;; draw vertical separator
  (let* ((start-line (main-window-start-line *main-window*))
         (end-line (- (main-window-height *main-window*) 1))
         (middle (floor (/ (- (main-window-width *main-window*) 2) 2)))
         (available-lines (- (tree-number-of-lines) start-line)))
    (mvwvline (main-window-window *main-window*)
              1
              (1+ middle)
              (char-code #\|)
              (- (main-window-height *main-window*) 2))
    ;; draw all visible lines
    (dotimes (line (min available-lines
                        (- (main-window-height *main-window*) 2)))
      (let* ((entry (tree-entry-at-line (+ start-line line))))
             (print-entry-in-window entry (1+ line))
             (wrefresh (main-window-window *main-window*))))))


;;; main-view.lisp ends here
