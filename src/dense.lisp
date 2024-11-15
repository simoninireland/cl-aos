;; Dense arrays
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


(defclass dense-array (array-schedule)
  ((data))
  (:documentation "A dense multidimensional array."))


;; ---------- Initialisation----------

(defmethod initialize-instance :after ((a dense-array)
				       &key
					 (element-type 'number)
					 (initial-element 0)
					 shape
					 &allow-other-keys)
  (let ((d (make-array shape
		       :element-type element-type
		       :initial-element initial-element
		       :adjustable nil)))
    (setf (slot-value a 'data) d)))


;; ---------- Construction ----------

(defmethod make-array-like ((a dense-array))
  (make-instance 'dense-array :shape (shape a)))


(defmethod const ((a dense-array) val)
  (dolist (loc (indices a))
    (setf (element a loc) val)))


(defmethod diag ((a dense-array) val)
  (let ((d (order a))
	(l (apply #'min (shape a))))
    (dolist (i (iota l))
      (let ((loc (make-list (list d) :initial-element i)))
	(setf (element a loc) val)))))


;; ---------- Operations ----------

;; In cl-aos indices are always lists

(defmethod element ((a dense-array) loc)
  (apply #'aref (cons (slot-value a 'data) loc)))


(defmethod (setf element) (v (a dense-array) loc)
  (setf (apply #'aref (cons (slot-value a 'data) loc)) v))


(defmethod indices ((a dense-array))
  (apply #'map-product (cons #'list (mapcar #'iota (shape a)))))


(defmethod .+ ((a dense-array) b)
  (ensure-same-shape a b)
  (let ((c (make-array-like a)))
    (dolist (loc (indices a))
      (let ((op (make-instance 'array-operation
			       :op #'+
			       :operands (list (make-instance 'array-index :a a :loc loc)
					       (make-instance 'array-index :a b :loc loc)))))
	(setf (calculation c loc) op)))))
