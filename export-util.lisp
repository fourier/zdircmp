;;; exporting-util.lisp --- Auxulary utilities for the cl-zdircmp app

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set of macros defining exporting symbols
;; Based on from http://clocc.sourceforge.net/clocc/src/ext/exporting/exporting.lisp

;;; Code:

(defpackage :zdircmp.util.export
  (:use :common-lisp)
  (:export :defconstant-export
           :defun-export
           :defmacro-export
           :defstruct-export
           ))

(in-package :zdircmp.util.export)


(defmacro defconstant-export (&whole whole
                                name initial-value &optional documentation)
  "Declares and exports the constant. Usage is the same as DEFCONSTANT"
  (declare (ignore initial-value documentation))
  `(progn
     (export ',(or name '(nil)))
     (defconstant ,name ,@(cddr whole))))

(defun function-block-name (name)
  (cond ((symbolp name) name)
        ((and (consp name) (eq (first name) 'setf)
              (consp (cdr name)) (symbolp (second name)) (null (cddr name)))
         (second name))
        (t (error "Not a function name: ~S" name))))

(defmacro defun-export (name lambda-list &body body)
  "Declares and exports the function. Usage is the same as DEFUN"
  `(progn
     (export ',(or (function-block-name name) '(nil)))
     (cl:defun ,name ,lambda-list ,@body)))

(defmacro defmacro-export (name lambda-list &body body)
  "Declares and exports the macro. Usage is the same as DEFMACRO"
  `(progn
     (export ',(or name '(nil)))
     (cl:defmacro ,name ,lambda-list ,@body)))

(defun concat-pnames (obj1 obj2)
  (let ((str (concatenate 'string (string obj1) (string obj2))))
    (if (and (plusp (length str)) (eql (char str 0) #\:))
        (intern (subseq str 1) (find-package "KEYWORD"))
        (intern str))))

(defun ds-accessor-name (slotname concname)
  (if concname 
      (concat-pnames concname slotname) 
      slotname))

(defmacro defstruct-export (name+options &rest slots)
  "Declares and exports the struct. Usage the same as DEFSTRUCT"
  (let ((name (if (consp name+options) (first name+options) name+options)))
    `(progn
       (export '(,name
                 ,@(let ((constructor-option-list nil)
                         (copier-option 0)
                         (predicate-option 0))
                     (when (consp name+options)
                       (dolist (option (rest name+options))
                         (if (or (eq option ':constructor) (equal option '(:constructor)))
                           (push (concat-pnames "MAKE-" name) constructor-option-list)
                           (when (and (consp option) (consp (cdr option)))
                             (case (first option)
                               (:constructor (push (second option) constructor-option-list))
                               (:copier (setq copier-option (second option)))
                               (:predicate (setq predicate-option (second option))))))))
                     (nconc (if constructor-option-list
                              (delete 'nil constructor-option-list)
                              (list (concat-pnames "MAKE-" name)))
                            (when copier-option
                              (list (if (eql copier-option 0)
                                      (concat-pnames "COPY-" name)
                                      copier-option)))
                            (when predicate-option
                              (list (if (eql predicate-option 0)
                                      (concat-pnames name "-P")
                                      predicate-option)))))
                 ,@(let ((conc-name-option 0))
                     (when (consp name+options)
                       (dolist (option (rest name+options))
                         (when (and (consp option) (consp (cdr option))
                                    (eq (first option) ':conc-name))
                           (setq conc-name-option (second option)))))
                     (when (eql conc-name-option 0)
                       (setq conc-name-option (concatenate 'string (string name) "-")))
                     (mapcar #'(lambda (slot-spec)
                                 (ds-accessor-name
                                   (if (consp slot-spec) (first slot-spec) slot-spec)
                                   conc-name-option))
                             slots))))
       (cl:defstruct ,name+options ,@slots))))


;;; export-util.lisp ends here
