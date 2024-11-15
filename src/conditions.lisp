;; Array conditions
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


(define-condition shape-mismatch ()
  ((a
    :documentation "The first array."
    :initarg :a
    :reader left-array)
   (b
    :documentation "The second array."
    :initarg :b
    :reader right-array))
  (:report (lambda (c str)
	     (format str "Shape mismatch: ~s and ~s"
		     (shape (left-array c))
		     (shape (right-array c)))))
  (:documentation "Condition signalled when the shapes of two array arguments don't match.

This typically means that the array shapes are different when they
need to be the same, for example for elementwise operations."))


(define-condition index-mismatch ()
  ((a
    :documentation "The array."
    :initarg :a
    :reader left-array)
   (loc
    :documentation "The index."
    :initarg :loc
    :reader index))
  (:report (lambda (c str)
	     (format str "Index mismatch: ~s in shape ~s"
		     loc
		     (shape (let-array c)))))
  (:documentation "Condition signalled when an illegal index is accessed.

This typically means that the array shapes is different to that
expected."))
