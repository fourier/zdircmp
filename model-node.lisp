;;; model-node.lisp --- diff model for directory trees

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

;; Diff model node

;;; Code:
(defpackage :zdircmp.model.node
  (:use :common-lisp :zdircmp.util :cl-fad)
  (:export :create-root-node
           ;; diff-node getters/setters
           :diff-node-parent
           :diff-node-left-path
           :diff-node-right-path
           :diff-node-short-name
           :diff-node-right-short-name
           :diff-node-children
           :diff-node-different
           ;; diff-node functions
           :diff-node-short-name-wrapper
           :diff-node-to-string
           :diff-node-is-directory
           :diff-node-side
           :diff-node-equal
           :diff-node-partial-rescan
           :diff-node-update-all-parents-diff
           :diff-node-update))


(in-package :zdircmp.model.node)

(declaim (optimize speed))

(defvar *message-fun* nil
  "Message function to report from model")

(defvar *activity-fun* nil
  "Activity function to report activity update from model")

(defun message (str)
  (when *message-fun* (funcall *message-fun* str)))

(defun update-activity ()
  (when *activity-fun* (funcall *activity-fun*)))


;; Create a struct diff-node with defined fielsd and getters/setters
;; here:
;; parent - parent node
;; left-path is the full path on the left side of the diff window,
;; right-path is the full path of the right side,
;; short-name - is the file or directory name
;; children - list of nodes - files or directories if the node is a directory
;; different = {nil, 'new, 'diff} - means comparison status
(defstruct diff-node parent left-path right-path short-name right-short-name children different)


(defun diff-node-to-string (node)
  (let* ((string-or-nil #'(lambda (x) (if x
                                          (cond ((stringp x) x)
                                                ((eq x 'new) "new")
                                                ((eq x 'diff) "different")
                                                (t (diff-node-short-name x)))
                                          "(empty)")))
         (children (diff-node-children node))
         (ch-str ""))
    (dolist (x children)
      (setq ch-str (concat ch-str "\n   * " (diff-node-short-name x))))
    (concat "Node: " (diff-node-short-name node)
            "\n"
            ;; " * Parent: " (let ((parent (diff-node-parent node)))
            ;;                 (if parent (diff-node-short-name parent) "nil"))
            " * Parent: " (funcall string-or-nil (diff-node-parent node))
            "\n"
            " * Left path: " (funcall string-or-nil (diff-node-left-path node))
            "\n"
            " * Right path: " (funcall string-or-nil (diff-node-right-path node))
            "\n"
            " * Children: " ch-str
            "\n")))


(defun diff-node-short-name-wrapper (node &optional right-side)
  (if (not right-side)
      (diff-node-short-name node)
      (diff-node-right-short-name node)))


(defun diff-node-is-directory (node)
  (let ((left (diff-node-left-path node))
        (right (diff-node-right-path node)))
    (if left
        (file-directory-p left)
        (file-directory-p right))))

(defun diff-node-side (node)
  (let ((left (diff-node-left-path node))
        (right (diff-node-right-path node)))
    (if (and left right) 'both
        (if left 'left 'right))))

(defun diff-node-equal (node1 node2)
  (and (string-equal (diff-node-short-name node1)
                     (diff-node-short-name node2))
       (string-equal (diff-node-left-path node1)
                     (diff-node-left-path node2))
       (string-equal (diff-node-right-path node1)
                     (diff-node-right-path node1))))

(defun files-equal (file1 file2)
  "Compare files using external diff. Returns t if equal"
  (let* ((f1 (namestring (file-exists-p file1)))
         (f2 (namestring (file-exists-p file2)))
         (cmd #+asdf3
              `(asdf/run-program:run-program (list "cmp" "-s" ,f1 ,f2) :ignore-error-status t :output nil :error-output nil)
              #-asdf3
              `(asdf:run-shell-command (format nil "cmp -s \"~a\" \"~a\"" ,f1 ,f2))))
    (eq (eval cmd) 0)))


(defun diff-node-partial-rescan (node)
  ;; assuming what parent is always exists
  ;; otherwise the UI shall force the full rescan
  (let ((isdir (diff-node-is-directory node))
        (left (diff-node-left-path node))
        (right (diff-node-right-path node)))
    ;; if node is a directory - traverse
    (when (and left right
               (file-exists-p left)
               (file-exists-p right))
      (if isdir 
          (let ((traverse (diff-node-traverse
                           node
                           left
                           right)))
            (setf (diff-node-different node) (car traverse))
            (setf (diff-node-children node) (cdr traverse)))
          ;; node is a file
          (setf (diff-node-different node)
                (if (files-equal left right)
                    nil
                    'diff))))))

(defun subtree (parent path side)
  "Creates a subtree for the given path for either 'left or 'right sides"
  (let ((files (directory-files path))
        (result nil))
    (dolist (file files)
      (if (file-directory-p file)
          (let* ((node (make-diff-node
                        :parent parent
                        :left-path (when (eq side 'left) file)
                        :right-path (when (eq side 'right) file)
                        :short-name (file-short-name file)
                        :right-short-name (file-short-name file)
                        :children nil
                        :different 'new))
                 (children (subtree node file side)))
            (setf (diff-node-children node) children)
            (push node result))
          (push (make-diff-node
                 :parent parent
                 :left-path (when (eq side 'left) file)
                 :right-path (when (eq side 'right) file)
                 :short-name (file-short-name file)
                 :right-short-name (file-short-name file)
                 :children nil
                 :different 'new)
                result)))
    result))

(defun diff-node-update-diff-from-children (node)
  (let ((children (diff-node-children node))
        (diff nil))
    (dolist (child children)
      (setq diff
            (update-diff
             diff
             (diff-node-different child))))
    (setf (diff-node-different node) diff)))

(defun diff-node-update-all-parents-diff (node)
  (let ((parent node))
    (loop while (setq parent (diff-node-parent parent)) do
         (diff-node-update-diff-from-children parent))))


(defun update-diff (old new)
  (if new
      (if (or (not old)
              (eq old 'new))
          new
          old)
      old))


(defun diff-node-traverse (parent path1 path2)
  "Function traversing 2 paths returning the list where the
first element is the difference status (nil, 'diff, 'new') and
the rest is the combined list of nodes"
  (let ((list1 (directory-files path1))
        (list2 (directory-files path2))
        (different-dir nil)
        (result nil))
    (declare (optimize speed) (list list1 list2))
    (update-activity)
    ;; first - adding all entries from left directory
    (dolist (file1 list1)
      ;; for every entry in the first directory 
      ;; we are creating the node
      (let* ((simple-name (file-short-name file1))
             (isdir (file-directory-p file1))
             (children nil)
             (different nil)
             ;; create the current node to be set as parent to
             ;; subdirectories
             (node (make-diff-node
                    :parent parent
                    :left-path file1
                    :right-path nil
                    :short-name simple-name
                    :right-short-name simple-name
                    :children nil
                    :different nil))
             ;; 1. find if the file is in the second directory and the type
             ;;    is the same - i.e. both are directories or both are files
             (file2 (find-if #'(lambda (x) (and (string-equal (file-short-name x)
                                                              simple-name)
                                                (eq isdir (file-directory-p x))))
                             list2)))
        ;; 2. if it is not in the second directory, add it as a node
        (if (not file2)
            (progn
              ;; 2.1 if it is a directory, add the whole subtree
              (when (file-directory-p file1)
                (setq children (subtree node file1 'left)))
              ;; 2.2 update the difference status for this entry
              (setq different 'new))
            ;; 3. if it is found in second directory and of the same type
            ;; 3.1 if it is a file
            (if (not (file-directory-p file1))
                (progn 
                  ;; 3.1.1 set difference status to this entry
                  (setq different (if (files-equal file1 file2) nil 'diff)))
                ;; 3.2 if it is the directory
                ;; 3.2.1 get the result of the directories comparison together with status
                (let ((traverse (diff-node-traverse node file1 file2)))
                  ;; 3.2.2 update the difference status for whole comparison from
                  ;;       difference result from the 2 subdirectories comparison
                  (setq different (car traverse))
                  ;; 3.2.3 set the children list from the 2 subdirectories comparison
                  (setq children (cdr traverse)))))
        ;; 2.3 update difference status for the whole comparison
        (setq different-dir (update-diff different-dir different))
        ;; update calculated parameters of the node
        (setf (diff-node-right-path node) file2)
        (setf (diff-node-children node) children)
        (setf (diff-node-different node) different)
        ;; push the created node to the result list
        (push node result)))
    ;; second - adding entries from the right directory which are not present
    ;; in the left directory
    (dolist (file2 list2)
      ;; for every entry in the second directory 
      ;; we are creating the node
      (let* ((simple-name (file-short-name file2))
             (isdir (file-directory-p file2))
             (children nil)
             ;; create the node to be added to the results list
             (node (make-diff-node
                    :parent parent
                    :left-path nil
                    :right-path file2
                    :short-name simple-name
                    :right-short-name simple-name
                    :children nil
                    :different 'new))
             ;; 1. find if the file is in the first directory and the type
             ;;    is the same - i.e. both are directories or both are files
             (file1 (find-if #'(lambda (x) (and (string-equal (file-short-name x)
                                                              simple-name)
                                                (eq isdir (file-directory-p x))))
                             list1)))
        ;; if it is not in the first directory, add it as a node
        (unless file1
          ;; if it is a directory, set the whole subtree to children
          (when (file-directory-p file2)
            (setq children (subtree node file2 'right)))
          ;; update the different status for the whole comparison
          (setq different-dir (update-diff different-dir 'new))
          ;; set calculated children to the node
          (setf (diff-node-children node) children)
          ;; push the created node to the result list
          (push node result))))
    ;; result is a pair: difference status and nodes list
    (cons different-dir result)))

(defun create-root-node (dir1 dir2 &key (message-function nil)
                                     (activity-function nil))
  (unless (file-directory-p dir1)
    (error (format nil "Path ~a is not a directory" dir1)))
  (unless (file-directory-p dir2)
    (error (format nil "Path ~a is not a directory" dir2)))
  (setf *message-fun* message-function)
  (setf *activity-fun* activity-function)
  (message (concat "Comparing " dir1 " and " dir2 " ..."))
  (let* ((model 
          (make-diff-node
           :parent nil
           :left-path dir1
           :right-path dir2
           :short-name (file-short-name dir1)
           :right-short-name (file-short-name dir2)
           :children nil
           :different nil))
         (traverse (diff-node-traverse model dir1 dir2)))
    (setf (diff-node-different model) (car traverse))
    (setf (diff-node-children model) (cdr traverse))
    (message "Done.")
    model))

(defun diff-node-update (node)
  (message (concat "Updating " (diff-node-short-name node) " ..."))
  (let ((traverse (diff-node-traverse node
                                      (diff-node-left-path node)
                                      (diff-node-right-path node))))
    (setf (diff-node-children node) (cdr traverse))
    (setf (diff-node-different node) (car traverse)))
  (message "Done."))

;; Examples:
;; (zdircmp.model.node::create-root-node "~/difftest/diff1" "~/difftest/diff2")
;; with optional message function:
;; (zdircmp.model.node::create-root-node "~/difftest/diff1" "~/difftest/diff2" :message-function #'(lambda (str) (princ str)))

;;; model-node.lisp ends here
