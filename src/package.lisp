;; Package definition
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

(in-package :common-lisp-user)

(defpackage cl-aos
  (:use :cl :alexandria)
  (:export
   ;; high-level array interface
   #:array-schedule
   #:order
   #:shape
   #:same-shape-p
   #:ensure-same-shape
   #:valid-index-p
   #:ensure-valid-index
   #:vector-p
   #:on-diagonal-p
   #:make-array-like
   #:const
   #:ones
   #:diag
   #:eye
   #:.+
   #:.-
   #:.*
   #:./
   #:@
   #:transpose
   #:indices
   #:element

   ;; operation schedules
   #:array-index-reference
   #:array-operation

   ;; dense arrays
   #:dense-array

   ;; conditions
   #:shape-mismatch
   #:index-mismatch
   ))
