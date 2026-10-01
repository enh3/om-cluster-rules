;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

;;; *************************************************************
;;; Copyright (C) 2017 Torsten Anders (torsten.anders@beds.ac.uk) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 3
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; *************************************************************

(in-package :cluster-rules)

;; (setf ccl::*pwgl-print-max-chars* 1000)

;;; ---------------------------------------------------------------------------
;;; Lambda-list introspection that tolerates lexical closures.
;;;
;;; OM 8.0 runs on LispWorks. Cluster Engine introspects every rule function it
;;; receives via CLUSTER-ENGINE::FUNCTION-LAMBDA-LIST, which calls
;;; CCL::FUNCTION-LAMBDA-LIST (the CCL-compat shim). For a lexical closure that
;;; shim signals "~S is a lexical closure." -- and Cluster Rules passes closures
;;; (lambdas capturing the rule's options) to CE rule applicators such as
;;; R-NOTE-METER, R-METER-NOTE and R-RHYTHM-RHYTHM. TU:ARGLIST (used in
;;; rhythm-rules.lisp to compute a rule's arity) can fail the same way.
;;;
;;; Fix: capture the original functions first, then install a tolerant version on
;;; BOTH CLUSTER-ENGINE::FUNCTION-LAMBDA-LIST and CCL::FUNCTION-LAMBDA-LIST, so
;;; every caller (present and future) benefits. Several introspection strategies
;;; are tried before giving up.
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Diagnostic file logging.
;;;
;;; The OM Listener shows neither printed output nor box-evaluation backtraces,
;;; so all diagnostics are appended to a log file instead.
;;; ---------------------------------------------------------------------------

(defvar *cr-debug-log*
  (let* ((here *load-pathname*)
         (dir (and here (pathname-directory here))))
    (if (and dir (equal (car (last dir)) "sources"))
        (merge-pathnames "debug.log"
                         (make-pathname :directory (butlast dir) :defaults here))
        (merge-pathnames "om-cluster-rules-debug.log" (user-homedir-pathname))))
  "File that receives all Cluster Rules diagnostics (append-only).")

(defun cr-log (fmt &rest args)
  "Append a timestamped line to *CR-DEBUG-LOG*. Never signals."
  (ignore-errors
    (with-open-file (s *cr-debug-log* :direction :output
                                      :external-format :utf-8
                                      :if-exists :append
                                      :if-does-not-exist :create)
      (multiple-value-bind (sec min hour date month year) (get-decoded-time)
        (format s "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D "
                year month date hour min sec))
      (apply #'format s fmt args)
      (terpri s)
      (finish-output s))))

(defun cr-log-backtrace (&optional (header "Backtrace:"))
  "Append a LispWorks backtrace of the current stack to *CR-DEBUG-LOG*."
  (cr-log "~A" header)
  (let* ((v1 (ignore-errors
               (with-output-to-string (out)
                 (dbg:output-backtrace :verbose :stream out))))
         (v2 (ignore-errors
               (with-output-to-string (out)
                 (dbg:output-backtrace t out))))
         (bt (cond ((and v1 (plusp (length v1))) v1)
                   ((and v2 (plusp (length v2))) v2)
                   (t (format nil "<backtrace unavailable> ~S ~S" v1 v2)))))
    (cr-log "~A" bt)))

(defmacro with-cr-error-log ((name) &body body)
  "Evaluate BODY, logging NAME and a backtrace of the failing stack to
*CR-DEBUG-LOG* if an error occurs. Uses HANDLER-BIND, which runs before the
stack unwinds, and does not transfer control, so the original error still
propagates unchanged after being logged."
  (let ((err (gensym "ERR")))
    `(handler-bind
         ((error (lambda (,err)
                   (cr-log "ERROR in ~A: ~A" ,name ,err)
                   (cr-log-backtrace (format nil "Backtrace for ~A:" ,name)))))
       (cr-log "entering ~A" ,name)
       ,@body)))

(defvar *orig-ccl-function-lambda-list*
  (let* ((pkg (find-package "CCL"))
         (sym (and pkg (find-symbol "FUNCTION-LAMBDA-LIST" pkg))))
    (and sym (fboundp sym) (symbol-function sym)))
  "Original CCL-compat function (fails on lexical closures), captured before redefinition.")

(defvar *orig-lw-function-lambda-list*
  #+lispworks (and (fboundp 'lw:function-lambda-list)
                   (symbol-function 'lw:function-lambda-list))
  #-lispworks nil
  "Original LW:FUNCTION-LAMBDA-LIST, captured as a fallback strategy.")

(defun cr-lambda-list (fn)
  "Return the lambda list of FN, tolerating lexical closures. Tries the original
CCL-compat function first, then CL:FUNCTION-LAMBDA-EXPRESSION, then
LW:FUNCTION-LAMBDA-LIST. Signals an error only if all strategies fail."
  (cr-log "cr-lambda-list: ~S | function-lambda-expression: ~S"
          fn (ignore-errors (cl:function-lambda-expression fn)))
  (dolist (probe (list (cons :orig-ccl-shim
                             (lambda () (funcall *orig-ccl-function-lambda-list* fn)))
                       (cons :cl-function-lambda-expression
                             (lambda ()
                               (let ((exp (cl:function-lambda-expression fn)))
                                 (if (and (consp exp) (eq (first exp) 'lambda))
                                     (second exp)
                                     (error "no lambda expression available")))))
                       (cons :lw-function-lambda-list
                             (lambda () (funcall *orig-lw-function-lambda-list* fn)))))
    (let ((res (handler-case (funcall (cdr probe))
                 (error (e)
                   (cr-log "  probe ~S failed: ~A" (car probe) e)
                   :cr-introspect-failed))))
      (unless (eq res :cr-introspect-failed)
        (cr-log "  probe ~S -> ~S" (car probe) res)
        (return-from cr-lambda-list res))))
  (cr-log "ALL PROBES FAILED for ~S" fn)
  (cr-log-backtrace "Backtrace of failed cr-lambda-list:")
  (error "Cannot determine the lambda list of ~S" fn))

(defun cluster-engine::function-lambda-list (fn)
  "Return the lambda list of FN, tolerating lexical closures (the original
implementation signals \"is a lexical closure\" for them)."
  (cr-lambda-list fn))

;; Install the tolerant version on the CCL-compat entry point as well, so that
;; direct callers of CCL::FUNCTION-LAMBDA-LIST are covered too.
(defvar *ccl-shim-status*
  (handler-case
      (let* ((pkg (find-package "CCL"))
             (sym (and pkg (find-symbol "FUNCTION-LAMBDA-LIST" pkg))))
        (cond ((null pkg) :no-ccl-package)
              ((null sym) :no-function-lambda-list-symbol)
              (t (let ((was-fbound (fboundp sym)))
                   (setf (symbol-function sym)
                         (lambda (fn) (cr-lambda-list fn)))
                   (if was-fbound :replaced-existing :installed-new)))))
    (error (e)
      (cr-log "CCL FUNCTION-LAMBDA-LIST install failed: ~A" e)
      :install-error))
  "Status of installing the tolerant version on CCL::FUNCTION-LAMBDA-LIST.")

;; Wrap LW:FUNCTION-LAMBDA-LIST so that any caller passing a closure gets a
;; logged error with a backtrace (silent failures were impossible to diagnose).
(defvar *lw-shim-wrapped*
  #+lispworks
  (handler-case
      (if (fboundp 'lw:function-lambda-list)
          (progn
            (setf (symbol-function 'lw:function-lambda-list)
                  (lambda (fn)
                    (handler-case (funcall *orig-lw-function-lambda-list* fn)
                      (error (e)
                        (cr-log "ERROR in lw:function-lambda-list: ~A" e)
                        (cr-log-backtrace "Backtrace of lw:function-lambda-list:")
                        (error e)))))
            t)
          :not-fbound)
    (error (e)
      (cr-log "LW FUNCTION-LAMBDA-LIST wrap failed: ~A" e)
      :install-failed))
  #-lispworks nil
  "T when LW:FUNCTION-LAMBDA-LIST was wrapped for diagnostic logging.")

;; CE's COMPILE-IF-NOT-COMPILED compiles every rule that is not already a
;; compiled function: (if (compiled-function-p expr) expr (compile name expr)).
;; LispWorks' COMPILE refuses lexical closures with "~S is a lexical closure."
;; because it cannot rebuild the captured environment -- which is exactly what
;; the rule closures built by this library are. Use such rules as-is (they stay
;; interpreted) instead of failing.
(defvar *orig-compile-if-not-compiled*
  (let ((s (find-symbol "COMPILE-IF-NOT-COMPILED"
                        (find-package "CLUSTER-ENGINE"))))
    (and s (fboundp s) (symbol-function s)))
  "Original CE function that signals on lexical closures, captured before redefinition.")

(defun cluster-engine::compile-if-not-compiled (name expr)
  "As the original Cluster Engine function, but tolerates lexical closures: if
COMPILE fails, the rule is used uncompiled (interpreted) instead of signaling."
  (if (compiled-function-p expr)
      expr
      (handler-case (compile name expr)
        (error (e)
          (cr-log "compile-if-not-compiled: using uncompiled rule ~S (~A)"
                  expr e)
          expr))))

(cr-log "=== om-cluster-rules: utils.lisp loaded ===")
(cr-log "  log file: ~A" *cr-debug-log*)
(cr-log "  original CCL shim captured: ~S"
        (and *orig-ccl-function-lambda-list* t))
(cr-log "  CCL::FUNCTION-LAMBDA-LIST install status: ~S" *ccl-shim-status*)
(cr-log "  original LW function-lambda-list captured: ~S"
        (and *orig-lw-function-lambda-list* t))
(cr-log "  LW::FUNCTION-LAMBDA-LIST wrapped: ~S" *lw-shim-wrapped*)
(cr-log "  original CE compile-if-not-compiled captured: ~S"
        (and *orig-compile-if-not-compiled* t))




(defun cluster-engine (no-of-variables rules metric-domain list-of-domains
                      &key (rnd? T) (debug? nil))
  "Slight variant of function cluster-engine::clusterengine where the order of variables is rearranged for shorter function calls. See the orig definition for further documentation."
  (cluster-engine::clusterengine no-of-variables
                                 rnd?
                                 debug?
                                 rules
                                 metric-domain
                                 list-of-domains))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Harmony and pitches 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; scale->pitchdomain

(defun scale->pitchdomain (scale-pitches &key (min 60) (max 72))
  "Expects a list of pitches representing a scale (either pitch classes or absolute pitches), and a minimum and maximum pitch. Returns a pitch domain for clusterengine that contains all pitches between the min and max in the scale."
  (let ((pcs (sort (mapcar #'(lambda (p) (mod p 12)) scale-pitches) #'<)))
    (loop for pitch from min to max 
       when (member (mod pitch 12) pcs)
       collect (list pitch))))


;;;
;;; Aux harmony defs
;;;

(defmethod pitch->pc ((pitch integer))
  "Returns the pitch class (int) in 12-TET of the given MIDI note number (int)."
  (mod pitch 12))

(defmethod pitch->pc ((pitches list))
  "Returns the pitch classes (list of ints) in 12-TET of the list of given MIDI note number (list of int)."
  (mapcar #'pitch->pc pitches))

(defun pc-member (pitch1 pitches2)
  "Returns T if the PC of pitch1 is a member in the PC set of pitches2."
  (member (mod pitch1 12) (pitch->pc pitches2)))

; (pc-member 60 '(48 57))

; (defun PC-member-save (pitch1 pitches2)
;  "Returns T if the PC of pitch1 is a member in the PC set of pitches2. Rests (i.e. pitches that are nil) are taken care of (e.g., if pitch1 is nil then PC-member returns T."
;  (if pitch1
;      (member (mod pitch1 12) 
;              (mapcar #'(lambda (p) (mod p 12))
;                      (remove NIL pitches2)))
;    T))

(defun transpose-pc (pc pc-interval)
  "Transposes the pitch class pc (int) by the pitch class interval pc-interval (int) such that the result is still a pitch class (int)."
  (mod (+ pc pc-interval) 12))

; (transpose-pc 7 7)


(defun pc-interval (pc1 pc2)
  "Returns the pitch class interval (int between 0 and 11) between the two pitch classes pc1 and pc2 (both ints)."
  (mod (- pc2 pc1) 12))

; (pc-interval 7 9)
; (pc-interval 7 2)
; (pc-interval 2 7)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; file-in-this-directory

(defparameter *current-directory* 
  (pathname-directory *load-truename*))

(defun file-in-this-directory (filename)
  "Returns a file in the same directory as the patch with the given filename (a string)."
  (make-pathname :directory *current-directory*
		 :name filename))

;; read-lisp-file

(defun read-lisp-file (path)
  "Expects a path name to a lisp file and returns the read (but not evaluated) content of the file. For example, if the file ; contains (1 2 3) then the list (1 2 3) is return (i.e., not a string, but a list, but 1 is not called as a function). Note that only the 1st Lisp form in path is returned."
  (if (probe-file path)
      (with-open-file (stream path)
	(read stream))
      (warn "path ~A does not exist!" path)))


(defun pprint-to-file (path expr)
  "Pretty-prints to the given path."
  (with-open-file (my-stream path
			     :direction :output
			     :if-exists :supersede)
    (pprint expr my-stream))
  path)


;;; TODO: decide -- do I actually still need this? If so, then port to plain CL.
;; read-harmony-file

#|
(defclass read-harmony-file-box (PWGL-box) ())

(defmethod patch-value ((self read-harmony-file-box) output)
  (let ((pos (position output (pwgl-outputs self)) ))
    (nth-output-patch-value self pos)))

(flet ((get-durs (plist dur-factor dur-key)
		 (let ((my-beat (getf plist :beat)))
		   (list (mapcar #'(lambda (dur) (* (/ dur my-beat) dur-factor)) (getf plist dur-key)))))
       (get-pitches (plist start-pitch pitch-key)
		    (list
		     (mapcar #'(lambda (pcs) 
				 (mapcar #'(lambda (pc) (+ pc start-pitch)) pcs)) 
			     (getf plist pitch-key)))))
  (defmethod nth-output-patch-value ((self read-harmony-file-box) (out (eql 0)))
    "scaledurs"
    (get-durs (read-lisp-file (nth-patch-value self 0))
	      (nth-patch-value self 1)
	      :scaledurs))
  (defmethod nth-output-patch-value ((self read-harmony-file-box) (out (eql 1)))
    "scalepitches"
    (get-pitches (read-lisp-file (nth-patch-value self 0))
		 (nth-patch-value self 2)
		 :scalepcss))   
  (defmethod nth-output-patch-value ((self read-harmony-file-box) (out (eql 2)))
    "chorddurs"
    (get-durs (read-lisp-file (nth-patch-value self 0))
	      (nth-patch-value self 1)
	      :chorddurs))
  (defmethod nth-output-patch-value ((self read-harmony-file-box) (out (eql 3)))
    "chordpitches"
    (get-pitches (read-lisp-file (nth-patch-value self 0))
		 (nth-patch-value self 2)
		 :chordpcss))) 


;; actual box def with multiple outputs
(PWGLDef read-harmony-file ((path NIL) ; (nth-patch-value self 0)
			    (dur-factor 1) ; (nth-patch-value self 1)
			    (pc-transposition 60)) ; (nth-patch-value self 2)
  "Retrieves predefined harmony data from a lisp file for harmonic rules like only-scale-PCs and only-chord-PCs. This file must contain a list (as first lisp value) with the following format: a keyword-value list (plist) with the keys

  :beat (an int; defines subdivision of a beat, usually a quarter note. E.g., :beat 4 means that 2 indicates an eighth note)
  :scaledurs (list of ints; the durations of underlying scales)
  :scalepcss (list of list of ints; each sublist contains the set of scale pitch classes, root is first int)
  :chorddurs (list of ints, the durations of underlying chords)
  :chordpcss (list of list of ints; each sublist contains the set of chord pitch classes, root first int).

Intended use: connect the outputs to the inputs of clusterengine: scaledurs/scalepitches to the rhythm- and pitch domain of the 1st part and chorddurs/chordpitches to the 2nd part.

Args:
path: path to lisp file with format above.

dur-factor: fraction by which all chord/scale durations are multiplied.

pc-transposition: integer added to all chord/scale pitch classes. Useful for displying the result from clusterengine in a chord editor."
  (:class 'read-harmony-file-box :outputs '("scaledurs" "scalepitches" "chorddurs" "chordpitches")
   :groupings '(1 2))
  ())
|#


#|
;;
;; Demo: How to define a box with multiple outputs
;; Def by Mikael Laurson, 18 May 2013 at pwgl-users@siba.fi
;;

(defclass my-multiout-box (PWGL-box) ())

(defmethod patch-value ((self my-multiout-box) output)
  (let ((pos (position output (pwgl-outputs self))))
    (nth-output-patch-value self pos)))

;; def for 1st output 
(defmethod nth-output-patch-value ((self my-multiout-box) (out (eql 0)))
  (+ (nth-patch-value self 0) (nth-patch-value self 1))); sum in1 in2

;; def for 2nd output
(defmethod nth-output-patch-value ((self my-multiout-box) (out (eql 1)))
  (+ (nth-patch-value self 0) (nth-patch-value self 1) (nth-patch-value self 2))) ; sum in1 in2 in3

;; ...
(defmethod nth-output-patch-value ((self my-multiout-box) (out (eql 2)))
  (+ (nth-patch-value self 0) (nth-patch-value self 2))) ; sum in1 in3

;; actual box def
(PWGLDef multiout-box ((a 0) (b 1) (c 2))
  "test multiout"
  (:class 'my-multiout-box :outputs '("1+2" "1+2+3" "1+3"))
  ())

|#


;;; TODO: decide -- do I actually still need this? If so, then port to plain CL.
;;;
;;; Utils for file export
;;;

#|
(defparameter rule::*score-counter* 0 "Incrementing counter for output filename")
(PWGLDef output-filename ((filename NIL)
			  &optional
			  (sub-directory NIL)
			  (extension ".xml")
			  (increment? NIL))
	 "Generate a path in the directory of the patch (!) with the given filename and optional extension.

NOTE: This box can only be evaluated from within the top-level patch (although the actual box can be contained in an abstraction), because otherwise the path of the patch is not known.

Optional args: 
  sub-directory (string): a directory before the actual file name (which can also contain directories itself).
  extension (string or list of strings): A string specifying a file extension. If extension is a list of strings, then a list of pathes is returned.
  increment? (Boolean): whether or not a numeric increment should be added automatically at the end of the resulting filename."
	 ()
	 (progn
	   (when increment? (setf rule::*score-counter* (1+ rule::*score-counter*)))
	   (let ((filenames 
		  (mapcar #'(lambda (ext) 
			      (file-in-this-directory
			       (concatenate 'string sub-directory "/" filename 
					    (if increment?
						(concatenate 'string "-" (write-to-string rule::*score-counter*))
					      "")
					    ext)))
			  (if (listp extension) extension (list extension)))))
	     (if (listp extension) filenames (first filenames)))))
|#

#| ;; ENP support not possible without PWGL
(defvar *enp-path* (user-homedir-pathname))
(PWGLDef export-enp ((score NIL)
		     &optional
		     (path (capi:prompt-for-file
			    "Export ENP to:"
			    :pathname *enp-path*
			    :filters '("enp" "*.enp") :operation :save)))
	 "Exports an ENP score notation file of the given score.

Optional arguments:
  path (defaults to OS saving window): the path where to save the resulting file."
	 ()
	 (when path (setq *enp-path* path))
	 (with-open-file (s path :direction :output :if-exists :supersede)
			 (format s "~:@W" (ccl::collect-score-info score :score-notation))))
|#



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; List processing
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun map-pairwise (fn xs)
  "Collects the result of applying the binary function gn on all pairwise combinations of elements in xs, i.e.,11 ((fn xs1 xs2) .. (fn xs1 xsN) (fn xs2 xs3) .. (fn xsN-1 xsN))."
  (if xs 
      (let ((x1 (first xs))
	    (xr (rest xs)))
	(append (mapcar #'(lambda (x2) (funcall fn x1 x2)) xr)
		(map-pairwise fn xr)))
    NIL))

(defun mappend (func &rest inlists)
  "Apply func to each element of inlist and append the result."
  (apply #'append (apply #'mapcar func inlists)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun boolean->int (b)
  "Converts T into 1 and nil into 0: useful for porting some Strasheela constraints"
  (if B 1 0))
