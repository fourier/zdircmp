;;; util.lisp --- Auxulary utilities for the cl-zdircmp app

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

(defpackage :zdircmp.util
  (:use :common-lisp :cl-fad)
  (:export :printable-string   
           :file-short-name
           :car-atom
           :file-directory-p
           :directory-files
           :concat
           :curry
           ))

(in-package :zdircmp.util)


;; Platform detection.
;; Based on http://abcl.org/svn/trunk/abcl/build-abcl.lisp
(defun platform ()
  #-clisp
  (let ((software-type (software-type)))
    (cond ((search "Linux" software-type)
           :linux)
          ((or (search "Mac OS X" software-type) ; abcl
               (search "Darwin" software-type))  ; sbcl
           :darwin)
          ((search "Windows" software-type)
           :windows)
          (t
           :unknown)))
  #+clisp
  (cond ((member :win32 *features*)
         :windows)
        ((equal 0 (ext:run-shell-command "uname | grep -i darwin" :output nil))
         :darwin)
        ((equal 0 (ext:run-shell-command "uname | grep -i linux" :output nil))
         :linux)
        (t
         :unknown)))

(defparameter *platform* (platform))

(defparameter *file-separator-char*
  (if (eq *platform* :windows) #\\ #\/))

(defparameter *file-separator*
  (make-string 1 :initial-element *file-separator-char*))



(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos))) 

(defun printable-string (string)
  "Strip newline character from file names, like 'Icon\n'"
  (remove-if #'(lambda (x) (or (char= x #\return) (char= x #\linefeed))) string))

(defun file-short-name (file)
  "Base file/directory name"
  (let* ((path (string-right-trim *file-separator* file)) ; cut the last "/" from path
         (last-slash-pos (search *file-separator* path :from-end t))) ; last "/" position
    (if last-slash-pos                                   ; path has slashes
        (subseq path (1+ last-slash-pos))                ; cut from the slash position
        path)))                                          ; just a path otherwise


(defun car-atom (value)
  "Returns value if value is an atom, otherwise (car value) or nil.
Used since car-safe returns nil for atoms"
  (if (atom value) value (car value)))


(defun file-directory-p (filename)
  "Returns t if `filename' exists and is a directory, nil otherwise"
  (let ((dir-exists (directory-exists-p filename))
        (file-exists (file-exists-p filename)))
    (when (and (not dir-exists) (not file-exists))
      (error 'not-exists (format t "File ~a not exists" filename)))
    (if dir-exists t nil)))

(defun directory-files (dirname)
  (mapcar 'namestring (list-directory dirname)))

(defmacro concat (str &rest others)
  `(concatenate 'string ,str ,@others))


;; taken from http://common-lisp.net/project/bese/docs/arnesi/html/Higher_0020order_0020functions.html
(defun curry (function &rest initial-args)
  "Returns a function which will call FUNCTION passing it
  INITIAL-ARGS and then any other args.

 (funcall (curry #'list 1) 2) ==> (list 1 2)"
  (lambda (&rest args)
    (apply function (append initial-args args))))


;;; util.lisp ends here
