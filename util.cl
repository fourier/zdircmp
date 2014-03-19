;;; util.el --- Auxulary utilities for the ztree package

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

(defpackage :ztree.util
  (:use :common-lisp)
  (:export :printable-string   
           :file-short-name
           :newline-and-begin
           :car-atom
           :insert-with-face
           :file-directory-p
           ))

(in-package :ztree.util)

(require 'cl-ncurses)

(defun printable-string (string)
  "Strip newline character from file names, like 'Icon\n'"
  (remove-if #'(lambda (x) (or (char= x #\return) (char= x #\linefeed))) string))

(defun file-short-name (file)
  "Base file/directory name"
  (pathname-name (parse-namestring file)))


(defun newline-and-begin ()
  (error 'not-implemented "Not implemented newline-and-begin"))
;; (newline)
;; (beginning-of-line))

(defun car-atom (value)
  "Returns value if value is an atom, otherwise (car value) or nil.
Used since car-safe returns nil for atoms"
  (if (atom value) value (car value)))


(defun insert-with-face (text face)
  "Insert text with the face provided"
  (error 'not-implemented "Not implemented insert-with-face"))
;; (let ((start (point)))
;;   (insert text)
;;   (put-text-property start (point) 'face face)))
  
(defun file-directory-p (filename)
  "Returns t if `filename' exists and is a directory, nil otherwise"
  (when (probe-file filename)
    (pathname-name filename)))

;;; util.cl ends here
