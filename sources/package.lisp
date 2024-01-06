;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

;;; *************************************************************
;;; Copyright (C) 2013 Torsten Anders (torsten.anders@beds.ac.uk) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 3
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; *************************************************************

;; use :string-tools seems not to work
(unless (find-package :ta-utilities)
  (make-package :ta-utilities
                :nicknames '(:ta-utils :tu)
                :use '(:common-lisp)))

(defpackage :fenv
  (:nicknames :fe :fenv)
  (:use :common-lisp)
  (:export :list->fenv
           :fenv?
           :y
           :fenv->list
           ))


(require-library "Cluster-Engine")

(defpackage :cluster-rules
  (:nicknames :rule)
  (:use :common-lisp :cluster-engine)) ;:pw <== there is no :pw package in OM))



;(eval-when (:execute :compile-toplevel :load-toplevel) <=== Specific for PWGL
;  (import '(ccl::PWGLdef ccl::patch-value ccl::PWGL-box ccl::nth-patch-value ccl::pwgl-outputs ccl::add-PWGL-user-menu)
;	  :cluster-rules))



