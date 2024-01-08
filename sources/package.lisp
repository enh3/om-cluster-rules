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
  (:use :common-lisp) ;:cluster-engine)
  (:import-from :cluster-engine ;added the :import-from (paulo)
   :rules->cluster
   :metric-domain 
   :R-predefine-meter 
   :Stop-rule-time 
   :Stop-rule-index 
   :preferences
   :R-rhythms-one-voice 
   :R-index-rhythms-one-voice 
   :R-pitches-one-voice 
   :R-index-pitches-one-voice 
   :R-time-signatures 
   :R-index-time-signatures 
   :R-only-m-motifs 
   :R-rhythms-one-voice-at-timepoints 
   :HR-rhythms-one-voice 
   :HR-index-rhythms-one-voice 
   :HR-pitches-one-voice 
   :HR-index-pitches-one-voice 
   :HR-time-signatures 
   :HR-index-time-signatures 
   :R-pmc-one-voice 
   :R-jbs-one-voice 
   :set-end
   :R-rhythm-pitch-one-voice 
   :R-index-rhythm-pitch-one-voice 
   :R-metric-hierarchy R-note-meter 
   :R-meter-note  
   :R-mel-interval-one-voice 
   :HR-rhythm-pitch-one-voice 
   :HR-index-rhythm-pitch-one-voice 
   :HR-duration-meter HR-meter-duration
   :R-rhythm-rhythm 
   :R-rhythm-hierarchy 
   :R-canon 
   :HR-rhythm-rhythm
   :R-pitch-pitch 
   :R-chords 
   :R-list-all-events 
   :HR-pitch-pitch 
   :HR-list-all-events
   :apply-and 
   :apply-minus)
  ) ; :pw <== There is a :pw package in CE library (pw-common-language.lisp file)
    ; We can use the :import-from if needed as well (paulo)

;(eval-when (:execute :compile-toplevel :load-toplevel) <=== Specific for PWGL
;  (import '(ccl::PWGLdef ccl::patch-value ccl::PWGL-box ccl::nth-patch-value ccl::pwgl-outputs ccl::add-PWGL-user-menu)
;	  :cluster-rules))



