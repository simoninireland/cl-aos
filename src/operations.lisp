;; Array operations
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


;; ---------- The array class ----------

(defclass array-schedule ()
  ((dim
    :type integer
    :initarg :dim
    :documentation "The order (dimensions) of the array."
    :reader order)
   (shape
    :type integer
    :documentation "The shape of the array."
    :initarg :shape
    :reader shape)
   (calculation
    :documentation "The calculation of the values in the array."
    :initform '()))
  (:documentation "A multidimensional array."))


;; ---------- Initialisation----------

(defmethod initialize-instance :after ((a array-schedule)
				       &key (element-type number) shape
					 &allow_other_keys)
  (declare (ignore element-type))
  (setf (slot-value a 'dim) (length shape)))


;; ---------- Construction ----------

(defgeneric make-array-like (a)
  (:documentation "Return an array like A.

The new array will have the same type as A, and the same shape."))


(defgeneric const (a val)
  (:documentation "Populate array A with VAL in each element."))


(defun ones (a)
  "Populate array A with 1 in each element."
  (const a 1.0))


(defgeneric diag (a val)
  (:documentation "Populate array A with VAL on the main diagonal.

If A is not rectangular, the main diagonal will be fitted as far as
possible."))


(defun eye (a)
  "Populate array A as an identity array, with 1 on the main diagonal."
  (diag a 1.0))


;; ---------- Access ----------

(defgeneric indices (a)
  (:documentation "Return a list of all the indices of array A."))


;; ----------- Binay operations ----------


;; ---------- Sanity checks ----------

(defun vector-p (a)
  "Test whether array A is 1-dimensional."
  (= (order a) 1))


(defun same-shape-p (a b)
  "Test that arrays A and B have the same shape."
  (equal (shape a) (shape b)))


(defun ensure-same-shape (a b)
  "Ensure arrays A and B have the same shape."
  (unless (same-shape-p a b)
    (error 'shape-mismatch a b)))


(defun valid-index-p (a loc)
  "Test that LOC is a valid index in array A."
  (every (lambda (i l)
	   (and (>= i 0)
		(< i l)))
	 loc
	 (shape a)))


(defun ensure-valid-index (a loc)
  "Ensure LOC is a valid index into array A."
  (unless (valid-index-p a loc)
    (error 'index-mismatch a loc)))


(defun on-diagonal-p (a loc)
  "Test whether LOC is on the main diagonal of array A.

For this to be the case the elements in LOC have to be the same
and have to be less than the shortest elekent of the shape of A."
  (let ((l (apply #'min (shape a))))
    (and (every #'eql loc (cdr loc))
	 (every (lambda (i) (< i l)) loc))))
