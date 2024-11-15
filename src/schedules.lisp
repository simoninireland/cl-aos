;; Array operation schedules
;;
;; Copyright (c) 2024 Simon Dobson
;;
;; This file is part of cl-aos, exploring array operation scheduling.
;;
;; cl-aos is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; cl-aos is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with cl-aos. If not, see <http://www.gnu.org/licenses/gpl.html>.

(in-package :cl-aos)

(defclass array-index-reference ()
  ((array
    :documentation "The array being indexed."
    :initarg :a)
   (loc
    :documentation "The location."
    :initarg :loc))
  (:documentation "An index to a location within an array."))


(defmethod print-object ((o array-index-reference) str)
  (if *print-readably*
      ;; print the index as a Lisp constructor
      (format str "make-instance 'cl-aos:array-index :a ~s :loc ~s"
	      (slot-value o 'array)
	      (slot-value o 'loc))

      ;; print something more friendly
      (format str "~s[~s]"
	      (slot-value o 'array)
	      (slot-value o 'loc))))


(defclass array-operation ()
  ((operation
    :type symbol
    :documentation "The operation,."
    :initarg :op)
   (operands
    :documentation "The operands."
    :initarg :operands))
  (:documentation "An array operation.

An operation consists of a scalar operation performed on one or more
arrays of compatible shapes. The operands are either scalars,
index references into arrays, or other array operations."))
