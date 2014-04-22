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
  (:use ::common-lisp :cl-ncurses
        :ztree.util
        :ztree.model.node
        :ztree.model.tree
        :ztree.constants
        :ztree.ui.utils
        :ztree.ui.command)
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
is the side of the cursor - 'ztree.model.node::left or 'ztree.model.node::right"
  (line 0)
  (side 'ztree.model.node::left))

 
(defstruct main-window
  (window nil)
  (x 0)
  (y 0)
  (width 0)
  (height 0)
  (node nil)
  (start-line 0)
  (cursor (make-cursor))
  (command (make-instance 'user-command)))


(defvar *main-window* (make-main-window)
  "Main ncurses window")

(defmacro with-color (color &body body)
  "Shortcut for with-color-win for currert window"
  `(with-color-win (main-window-window *main-window*) ,color ,@body))

(defmacro defcommand (name &body body)
  "Declares the command function. Usage example:
`(defcommand up (let ((a 1)) a))
generates the function
`(DEFUN PROCESS-UP ()
  (SET-LAST-COMMAND 'UP (MAIN-WINDOW-COMMAND *MAIN-WINDOW*))
  (LET ((A 1))
    A))"
  (let ((command-fun-name (intern
                           (string-upcase
                           (concatenate 'string "process-" (symbol-name name))))))
  `(defun ,command-fun-name ()
     ,@body
     (set-last-command (quote ,name) (main-window-command *main-window*)))))


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


(defun window-line-to-tree-line (window-line)
  "Returns the line in the tree array by given WINDOW-LINE 0-based line in window."
  (+ (main-window-start-line *main-window*) window-line))

(defun tree-line-to-window-line (line)
  "Returns line in window by given tree array line.
May be negative or more than window size"
  (- line (main-window-start-line *main-window*)))

(defun line-number-at-pos ()
  (let* ((cursor-pos (cursor-line (main-window-cursor *main-window*)))
         (line (window-line-to-tree-line cursor-pos)))
    line))

(defun line-visible-p (line)
  "Returns t if the tree-line is visible in the current screen"
  (let ((window-line (tree-line-to-window-line line))
        (max-line (1- (main-window-height *main-window*))))
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

(defun scroll-lines (number-of-lines)
  "Scrolls the current window to NUMBER-OF-LINES"
  (let ((new-position (get-new-position number-of-lines
                                        (main-window-start-line *main-window*)
                                        (- (main-window-height *main-window*) 2)
                                        (tree-number-of-lines))))
    (setf (main-window-start-line *main-window*) new-position)
    (refresh-view)))

(defun scroll-to-line (line)
  "Set the cursor to the LINE, line is the line in tree(buffer),
not a screen line. If the line is visible (on screen), just set cursor to
this line, otherwise:
1. If the line is above the screen, set the line as a first line in
the screen
2. If the line is below the screen, set is as high as possible"
  ;; if line is visible set the cursor to this line
  (if (line-visible-p line)
      (setf (cursor-line (main-window-cursor *main-window*))
            (tree-line-to-window-line line))
      ;; otherwise scroll to the number of lines - works only
      ;; for scroll up for now. TODO: fix it for scroll down - case 2
      (let ((window-line (tree-line-to-window-line line)))
        (scroll-lines window-line)
        (setf window-line (tree-line-to-window-line line))
        (setf (cursor-line (main-window-cursor *main-window*))
              (tree-line-to-window-line line))))
  (refresh-view))

;; UP key event handler - move cursor up one line
(defcommand up
  (let ((cursor-pos (cursor-line (main-window-cursor *main-window*))))
    ;; if the cursor is not on the first line, just move it up
    (if (> cursor-pos 0)
        (progn
          (setf (cursor-line (main-window-cursor *main-window*)) (1- cursor-pos))
          (refresh-view))
        ;; otherwise - cursor is on the same place, but window moves up
        (scroll-lines -1))))

;; DOWN key event handler - move cursor down one line
(defcommand down
  (let ((cursor-pos (cursor-line (main-window-cursor *main-window*))))
    (when (/= (+ (main-window-start-line *main-window*) cursor-pos)
              (1- (tree-number-of-lines)))
      ;; if the cursor is on the last line, scroll down, cursor is on the same place
      (if (= cursor-pos (- (main-window-height *main-window*) 3))
          (scroll-lines 1)
          (progn
            ;; otherwise update its position
            (setf (cursor-line (main-window-cursor *main-window*)) (1+ cursor-pos))
            (refresh-view))))))

;; PAGE UP key event handler - scrolls up 1 screen if possible
(defcommand pgup
  (scroll-lines (- (main-window-height *main-window*))))

;; PAGE DOWN key event handler - scrolls down 1 screen if possible"
(defcommand pgdn
  (scroll-lines (main-window-height *main-window*)))

;; ENTER key event handler - opens/close directories"
(defcommand return
  (let* ((line (line-number-at-pos))
         (node (tree-entry-node (tree-entry-at-line line)))
         (expanded (node-expanded-p node))
         (node-is-file (not (diff-node-is-directory node))))
    (if (and node-is-file
             (eq (diff-node-side node) 'ztree.model.node::both))
        (let ((left (diff-node-left-path node))
              (right (diff-node-right-path node)))
          ;(message  "Comparing ~a and ~a" left right)
          (def-prog-mode)
          (endwin)
          (asdf/run-program:run-program (list "vimdiff" left right) :ignore-error-status t :output :interactive)
          (reset-prog-mode)
          (refresh))
        (toggle-expand-state-by-line line (not expanded)))))

;; TAB key event handler - jump to the other side of the window"
(defcommand tab
  (let ((side (cursor-side (main-window-cursor *main-window*))))
    (if (eq side 'ztree.model.node::left)
        (setf (cursor-side (main-window-cursor *main-window*))
              'ztree.model.node::right)
        (setf (cursor-side (main-window-cursor *main-window*))
              'ztree.model.node::left))
    (refresh-view)))

;; Action on Backspace key: to jump to the line of a parent node or
;; if previous key was Backspace - close the node
(defcommand backspace
  (let* ((line (line-number-at-pos))
         (entry (tree-entry-at-line line))
         (parent-line (tree-entry-parent-line entry)))
    ;; perform action on any node except the root node
    (when (/= parent-line line)
      ;; if it was 2 subsequent backspace commands - close the current node
      (if (and (equal (last-command (main-window-command *main-window*))
                                    'backspace)
               (= 0 (mod (repeat-count (main-window-command *main-window*)) 2)))
            (toggle-expand-state-by-line line nil)
            (scroll-to-line parent-line))))
  (refresh-view))

  

(defun process-key (key)
  "Keypress dispatcher"
  (cond ((eq key +KEY-UP+)
         (process-up))
        ((eq key +KEY-DOWN+)
         (process-down))
        ((eq key +KEY-LEFT+) 
         (message "LEFT"))
        ((eq key +KEY-RIGHT+) 
         (message "RIGHT"))
        ((eq key +KEY-ENTER+) 
         (process-return))
        ((eq key +KEY-SPACE+) 
         (message "SPACE"))
        ((key-backspace-p key)
         (process-backspace))
        ((eq key +KEY-HOME+) 
         (message "HOME"))
        ((eq key +KEY-END+) 
         (message "END"))
        ((eq key +KEY-TAB+) 
         (process-tab))
        ((eq key +KEY-NPAGE+) 
         (process-pgdn))
        ((eq key +KEY-PPAGE+)
         (process-pgup))
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
         (message "Pressed: ~a" key)))
  (refresh-view))

(defun set-model-node (node)
  "Sets the current root node of the model. Refreshes the window
and redraws all data inside"
  (setf (main-window-node *main-window*) node)
  (refresh-tree node)
  (refresh-view))


(defun print-entry-in-window (line)
  (let* ((entry (tree-entry-at-line line))
         (node (tree-entry-node entry))
         (offset (tree-entry-offset entry))
         (side (diff-node-side node))
         (short-name (if (or (eq side 'ztree.model.node::both)
                             (eq side 'ztree.model.node::left))
                         (diff-node-short-name node)
                         (diff-node-right-short-name node)))
         (expandable (diff-node-is-directory node))
         (expanded   (node-expanded-p node))
         (diff       (diff-node-different node))
         (parent-line (tree-entry-parent-line entry)))
    (if (eq side 'ztree.model.node::both)
        ;; insert left AND right labels
        (progn 
          (insert-single-entry short-name
                               expandable
                               expanded
                               line
                               parent-line
                               offset
                               'ztree.model.node::left
                               diff)
          (insert-single-entry short-name
                               expandable
                               expanded
                               line
                               parent-line
                               offset
                               'ztree.model.node::right
                               diff))
        (progn
          (insert-single-entry short-name
                               expandable
                               expanded
                               line
                               parent-line
                               offset
                               side
                               diff)
          (insert-dummy-cursor-opposite-side (1+ (tree-line-to-window-line line))
                                             offset
                                             side)))))

(defun insert-dummy-cursor-opposite-side (window-line offset side)
  (let* ((middle (floor (/ (- (main-window-width *main-window*) 2) 2)))
         (x-position (+ +left-offset+
                        (* offset 4)
                        (if (eq side 'ztree.model.node::left) middle 0)))
         (win (main-window-window *main-window*)))
    (when (and (= (1- window-line)
                  (cursor-line (main-window-cursor *main-window*)))
               t)
      (with-color :black-on-white
        (mvwprintw win window-line x-position " ")))))



(defun color-for-diff (diff)
  (cond ((eq diff 'ztree.model.node::diff) :red)
        ((eq diff 'ztree.model.node::new)  :blue)
        (t :white)))

(defun inverse-color (color)
  (cond ((eq color :red) :red-on-white)
        ((eq color :blue) :blue-on-white)
        ((eq color :green) :green-on-white)
        (t :black-on-white)))

(defun insert-single-entry (short-name
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
  (let* ((middle (floor (/ (- (main-window-width *main-window*) 2) 2)))
         ;; x-position is the TEXT position. So the [+] sign position or tree leaf width
         ;; shall be calculated by subtracting from the x-position
         (x-position (+ +left-offset+     ; 1) offset from window left side (to move out of border)
                        (* (1+ offset) 4) ; 2) 4 chars for possible [+], and 4 chars per offset
                                        ; 3) shift to the middle for the right-side nodes
                        (if (eq side 'ztree.model.node::right) (1+ middle) 0))) 
         (win (main-window-window *main-window*))
         (height (main-window-height *main-window*))
         (window-line (1+ (tree-line-to-window-line line)))
         (parent-window-line (tree-line-to-window-line parent-line)))
    (flet ((node-sign (exp x y)
             (let ((text (format nil "[~a]" (if exp "-" "+"))))
               (with-color :white
                 (mvwprintw win y x text)))))
      ;; x-position is a text position, so in order to draw the line and
      ;; write a [+] sign we need to shift it to the left, but don't forget
      ;; the offset from the border
      (let ((line-start (- x-position 4 +left-offset+))
            (line-length 5))
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
            (mvwhline win window-line line-start  ACS_HLINE line-length))
          ;; draw vertical line
          ;; first draw the ` character in the bottom of vertical line
          ;; any other nodes below will overwrite it with "|" anyways
          (when (and (> window-line 0)            ; don't draw on top border
                     (< window-line (1- height))) ; don't draw on bottom border
            (mvwaddch win window-line (1- line-start) ACS_LLCORNER))
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
            (mvwvline win
                      start-line        ; y start position
                      (1- line-start)   ; x position
                      ACS_VLINE         ; character
                      len))))           ; number of rows to draw
      ;; determine if the line is under the cursor
      (let ((color (color-for-diff diff)))
        (when (and (= (1- window-line)
                      (cursor-line (main-window-cursor *main-window*)))
                   (eq (cursor-side (main-window-cursor *main-window*)) side))
          (setf color (inverse-color color)))
        ;; guard against out-of window
        (when (and (> window-line 0)            ; don't draw on top border
                   (< window-line (1- height))) ; don't draw on bottom border
          (with-color color
            (mvwprintw win window-line x-position short-name)))))))


(defun refresh-contents ()
  "Redraws all window's contents - tree and separator"
  ;; refresh tree in memory
  (refresh-tree (main-window-node *main-window*))
  ;; draw vertical separator
  (let* ((middle (floor (/ (- (main-window-width *main-window*) 2) 2))))
    (mvwvline (main-window-window *main-window*)
              1
              (1+ middle)
              (char-code #\|)
              (- (main-window-height *main-window*) 2))
    ;; draw all visible lines
    (dotimes (line (tree-number-of-lines))
      (print-entry-in-window line)))
  (wrefresh (main-window-window *main-window*)))


;;; main-view.lisp ends here
