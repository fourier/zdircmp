;;; model-tree.lisp --- model tree for directory trees

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

;; Diff model tree

;;; Code:
(defpackage :ztree.model.tree
  (:use :common-lisp :ztree.util :ztree.model.node)
  (:export
   :refresh-tree
   :tree-number-of-lines
   :tree-entry-at-line
   :tree-entry-node
   :tree-entry-parent-line
   :tree-entry-offset
   :node-expanded-p
   :toggle-expand-state-by-line
   ))


(in-package :ztree.model.tree)


(defstruct model-tree
  "In-memory tree representation of the data model.
Contains an information about lines in text buffer and corresponding
nodes and their statuses - expanded or not, how deep, which line contains
the parent etc.
Fields:
ENTRIES - Array of line-to-node mappings: index - 0-based line number,
          value - corresponding tree-entry
EXPANDED - hash table of expanded nodes. If node is expanded it is added
           to the hash table with key 'node' and value 't' otherwise the node
           is removed from the hash"
  (entries (make-array 0 :adjustable t :fill-pointer t))
  (expanded (make-hash-table)))
           

(defstruct (tree-entry
             (:print-function (lambda (entry stream depth)
                                (declare (ignore depth))
                                (dotimes (x (tree-entry-offset entry))
                                  (format stream " "))
                                (format stream "~A" (diff-node-short-name (tree-entry-node entry))))))
  "Represents the entries in the model tree. Every entry is the line/node
in the tree representation.
Fields:
NODE - node itself
PARENT-LINE -index of the line corresponding to the parent node
OFFSET - offset to the left from the beginning of the line (depth)"
  node
  (parent-line 0)
  (offset 0))


(defvar *model* (make-model-tree)
  "Tree model")

(defun tree-number-of-lines ()
  "Number of lines in the tree"
  (length (model-tree-entries *model*)))

(defun tree-entry-at-line (line)
  "Returns the tree entry for a given line"
  (elt (model-tree-entries *model*) line))

    
(defun refresh-tree (root)
  "Refreshes the tree representing lines and their corresponding nodes"
  ;; save the old list of expanded nodes
  (let ((expanded-list
         (if (or
              ;; if old was empty
              (= (length (model-tree-entries *model*)) 0)
              ;; or root is changed 
              (and (> (length (model-tree-entries *model*)) 0)
                   (not (eql (tree-entry-node (aref (model-tree-entries *model*) 0)) root))))
             ;; nuke it!
             (make-hash-table)
             ;; otherwise store previous
             (model-tree-expanded *model*))))
    ;; recreate the model
    (setf *model* (make-model-tree))
    ;; restore the previous list of expanded nodes
    (setf (model-tree-expanded *model*) expanded-list)
    ;; make sure that the root is expanded
    (toggle-expand-state root t)
    ;; and create a tree
    (insert-node-contents root 0 0)))

(defun collapse-all ()
  (setf (model-tree-expanded *model*) (make-hash-table)))

(defun node-expanded-p (node)
  (if (gethash node (model-tree-expanded *model*)) t nil))


(defun toggle-expand-state (node do-expand)
  "Set the expanded state of the NODE to DO-EXPAND(nil or t)"
  (when (diff-node-is-directory node)
    (setf (gethash node (model-tree-expanded *model*)) do-expand)))


(defun toggle-expand-state-by-line (line do-expand)
  "Set the expanded state of the node in LINE to DO-EXPAND(nil or t)"
  ;; sanity check
  (assert (and (>= line 0)
               (< line (length (model-tree-entries *model*)))))
  (toggle-expand-state (tree-entry-node (aref (model-tree-entries *model*) line)) do-expand))

(defun get-splitted-node-children (node)
  "Returns pair of 2 elements: list of expandable nodes and
list of leafs"
  (let ((nodes (diff-node-children node)))
    (flet ((comp (x y)
             (string< (diff-node-short-name x)
                      (diff-node-short-name y))))
      (cons (sort (copy-list (remove-if-not 'diff-node-is-directory nodes)) #'comp)
            (sort (copy-list (remove-if 'diff-node-is-directory nodes)) #'comp)))))


(defun insert-node-contents (node offset parent-line)
  "Insert the node itself to the line with given parent and offset. If node
is expanded, insert its children in a sorted way"
  (let ((new-line 
         (vector-push-extend (make-tree-entry :node node :parent-line parent-line :offset offset) (model-tree-entries *model*))))
  (when (node-expanded-p node)
    (let* ((contents (get-splitted-node-children node))
           ;; contents is the list of 2 elements:
           (nodes (car contents))     ; expandable entries - nodes
           (leafs (cdr contents))     ; leafs - which doesn't have subleafs
           (new-offset (1+ offset)))  ; increase the offset since it is a child
      ;; auxulary function which iterates over the list of nodes
      ;; and inserts them
      (flet ((insert-contents (nds)
               ;; iterate by index - index will be the relative line
               (dotimes (index (length nds))
                 (insert-node-contents (nth index nds)
                                       new-offset
                                       new-line))))
        ;; first insert directories
        (insert-contents nodes)
        ;; next insert files
        (insert-contents leafs))))))



;;; model-tree.lisp ends here
