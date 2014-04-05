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
  ;; import message function from ztree.view.message for easy use of messages
  ;;(:import-from :ztree.view.message :message)
  (:export
   :refresh-tree
   ))


(in-package :ztree.model.tree)


(defstruct model-tree entries expanded)

(defstruct tree-entry node parent-line)


(defvar *model* (make-model-tree)
  "Tree model")

(defun refresh-tree (root)
  (setf (model-tree-entries *model*) (make-array 1 :adjustable t :fill-pointer t))
  (setf (aref (model-tree-entries *model*) 0) (make-tree-entry :node root :parent-line 0))
  (setf (model-tree-expanded *model*) (make-hash-table)))


