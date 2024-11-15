;; Dense array tests
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

(in-package :cl-aos/test)
(in-suite cl-aos)


;; ---------- Access and construction ----------

(test test-dense-1d
  "Test creating a 1d dense array."
  (let ((a (make-instance 'dense-array :shape '(3))))
    (is (equal (shape a) '(3)))
    (is (equal (order a) 1))
    (is (equal (indices a) '((0) (1) (2))))
    (dolist (i (indices a))
      (is (= (element a i) 0)))))


(test test-dense-2d
  "Test creating a 2d dense array."
  (let ((a (make-instance 'dense-array :shape '(3 3))))
    (is (equal (shape a) '(3 3)))
    (is (equal (order a) 2))
    (is (equal (indices a) '((0 0) (0 1) (0 2)
			     (1 0) (1 1) (1 2)
			     (2 0) (2 1) (2 2))))
    (dolist (i (indices a))
      (is (= (element a i) 0)))))


(test test-dense-const
  "Test creating an array filled with a constant."
  (let ((a (make-instance 'dense-array :shape '(2 2))))
    (const a 5)
    (dolist (i (indices a))
      (is (= (element a i) 5)))))


(test test-on-diagonal
  "Test we can determine that an index is or isn't on a main diagonal."
  ;;; redo to filter on- and off-diagonal indices

  (let ((a (make-instance 'dense-array :shape '(3 3))))
    (is (on-diagonal-p a '(0 0)))
    (is (on-diagonal-p a '(1 1)))
    (is (on-diagonal-p a '(2 2)))
    (is (not (on-diagonal-p a '(2 3))))
    (is (not (on-diagonal-p a '(4 4)))))

  ;; tensor

  ;; non-square matrix
  (let ((a (make-instance 'dense-array :shape '(3 4))))
    (is (on-diagonal-p a '(0 0)))
    (is (on-diagonal-p a '(1 1)))
    (is (on-diagonal-p a '(2 2)))
    (is (not (on-diagonal-p a '(2 3))))
    (is (not (on-diagonal-p a '(4 4)))))

  )



(test test-eye-square
  "Test we can create an identity matrix."
  (flet ((on-diag (i)
	   "Test whether I is an indx on the main diagonal."
	   (every #'eql i (cdr i))))
    (let ((a (make-instance 'dense-array :shape '(3 3))))
      (eye a)
      (dolist (i (indices a))
	(is (= (element a i)
	       (if (on-diag i)
		   1
		   0)))

	)

      ))

  )


;; ---------- Schedules ----------
