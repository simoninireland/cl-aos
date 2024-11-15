;; System definitions
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

(asdf:defsystem "cl-aos"
  :description "Exploring array operation scheduling"
  :author "Simon Dobson <simon.dobson@st-andrews.ac.uk"
  :version (:read-file-form "version.sexp")
  :license "GPL3"
  :depends-on ("alexandria")
  :pathname "src/"
  :serial t
  :components ((:file "package")
	       (:file "dense")
	       (:file "schedules")
	       (:file "operations")
	       (:file "conditions")
	       )
  :in-order-to ((test-op (test-op "cl-aos/test"))))


(asdf:defsystem "cl-aos/test"
  :description "Test suite for CL-AOS."
  :depends-on ("alexandria" "cl-aos" "fiveam")
  :pathname "test/"
  :serial t
  :components ((:file "package")
	       (:file "test-dense-arrays")
	       )
  :perform (test-op (o c) (uiop:symbol-call :fiveam '#:run-all-tests)))
