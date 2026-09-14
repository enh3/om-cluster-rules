(in-package :om)

;; CLUSTER-RULES imports symbols from the Cluster-Engine package while
;; compiling sources/package.lisp, so the dependency must already be loaded.
(require-library "Cluster-Engine")

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc 'om::compile&load 
      (list
	   ;all ta-utilities lisp files in order they appear in the original ta-utilities.asd file
	   (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "package" :type "lisp")
	   (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources" "ta-utilities")) :name "my-utilities" :type "lisp")
	   (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources" "ta-utilities")) :name "export" :type "lisp")
	   (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "macros" :type "lisp")
	   (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "fenv" :type "lisp")
	  ; (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources" "ta-utilities")) :name "make-package" :type "lisp")
	  ; (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources" "ta-utilities")) :name "my-utilities" :type "lisp")
	  ; (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources" "ta-utilities")) :name "export" :type "lisp")
        ;all cluster-rules files in order they appear in :components in the original pwgl-cluster-rules.asd file (without the menus.lisp and export.lisp files, which is specific for PWGL)	 	   
	   ;(make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "package" :type "lisp")	
	   (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "utils" :type "lisp")
           (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "rhythm-rules" :type "lisp")
           (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "melody-rules" :type "lisp")
           (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "harmony-rules" :type "lisp")
           (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "counterpoint-rules" :type "lisp")
           (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "om-interface" :type "lisp")
       ))

; using "make-pathname" and *load-pathname*, allow us to put our library anywhere

;--------------------------------------------------
; Seting the menu and sub-menu structure, and filling packages
; The sub-list syntax:
; ("sub-pack-name" subpack-lists class-list function-list class-alias-list)
;--------------------------------------------------

(om::fill-library
 '(("Profile"
    (("Mappings" nil nil
      (om-cr::mp-add-offset om-cr::mp-multiply om-cr::mp-add-random-offset) nil)
     ("Transformations" nil nil
      (om-cr::trfm-scale om-cr::trfm-add-bpf om-cr::trfm-multiply-bpf om-cr::trfm-reverse) nil))
    nil
    (om-cr::follow-timed-profile-hr
     om-cr::follow-profile-hr
     om-cr::follow-interval-profile
     om-cr::rhythm-profile-bpf-hr
     om-cr::compose-functions)
    nil)

   ("Rhythm"
    (("Accent rules" nil nil
      (om-cr::mk-accent-has-at-least-duration-ar
       om-cr::mk-accent->-prep-and->=-dur-ar
       om-cr::mk-accent->-prep-or->=-dur-ar
       om-cr::thomassen-accents
       om-cr::thomassen-accents-ar) nil))
    nil
    (om-cr::no-two-consecutive-syncopations
     om-cr::no-syncopation
     om-cr::no-syncopation-unless-accented
     om-cr::only-simple-syncopations
     om-cr::only-simple-tuplet-offs
     om-cr::start-with-rest
     om-cr::metric-offset-of-motif
     om-cr::phrase-length
     om-cr::similar-sim-durations
     om-cr::metric-accents
     om-cr::accents-in-other-voice)
    nil)

   ("Melody" nil nil
    (om-cr::min/max-interval
     om-cr::set-pitches
     om-cr::set-intervals
     om-cr::prefer-interval-hr
     om-cr::accumulative-interval
     om-cr::no-direct-repetition
     om-cr::no-repetition
     om-cr::restrict-consecutive-directions
     om-cr::resolve-skips
     om-cr::durations-control-intervals
     om-cr::follow-profile-hr
     om-cr::follow-timed-profile-hr)
    nil)

   ("Harmony" nil nil
    (om-cr::only-scale-pcs
     om-cr::only-chord-pcs
     om-cr::only-spectrum-pitches
     om-cr::long-notes-chord-pcs
     om-cr::chord-tone-before/after-rest
     om-cr::chord-pc-at-1st-tone-hack
     om-cr::stepwise-non-chord-tone-resolution
     om-cr::chord-tone-follows-non-chord-tone
     om-cr::unequal-sim-pcs
     om-cr::number-of-sim-pcs
     om-cr::set-harmonic-intervals
     om-cr::min/max-harmonic-interval
     om-cr::tintinnabuli-m-voice
     om-cr::tintinnabuli-t-voice
     om-cr::set-chord-at-positions
     om-cr::set-root-at-positions
     om-cr::limit-voice-leading-distance
     om-cr::schoenberg-progression-rule
     om-cr::ascending-progression
     om-cr::resolve-descending-progression)
    nil)

   ("Counterpoint" nil nil
    (om-cr::no-voice-crossing
     om-cr::no-parallels)
    nil)

   ("Utilities" nil nil
    (om-cr::scale->pitchdomain
     om-cr::file-in-this-directory
     om-cr::read-lisp-file
     om-cr::pprint-to-file
     om-cr::map-pairwise
     om-cr::mappend)
    nil)))
