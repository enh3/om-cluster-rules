(in-package :om)

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
       ))

; using "make-pathname" and *load-pathname*, allow us to put our library anywhere

;--------------------------------------------------
; Seting the menu and sub-menu structure, and filling packages
; The sub-list syntax:
; ("sub-pack-name" subpack-lists class-list function-list class-alias-list)
;--------------------------------------------------

(om::fill-library 
 ;;; the structure of the menu is defined in the original menus.lisp file
 '(("Profile"    
    (("Mappings" nil nil (rule::mp-add-offset rule::mp-multiply rule::mp-add-random-offset) nil)
     ("Transformations" nil nil (rule::trfm-scale rule::trfm-add-BPF rule::trfm-multiply-BPF rule::trfm-reverse) nil))
    nil
    (rule::follow-timed-profile-hr
     rule::follow-profile-hr
        ; rule::follow-interval-profile
        ; rule::rhythm-profile-BPF-hr
     rule::compose-functions
     ) nil)

   ("Rhythm" 
    (("Accent rules" Nil Nil (rule::mk-accent-has-at-least-duration-ar 
                              rule::mk-accent->-prep-AND->=-dur-ar rule::mk-accent->-prep-OR->=-dur-ar
                              rule::thomassen-accents rule::thomassen-accents-ar
                              ) Nil))
    Nil (rule::no-two-consecutive-syncopations rule::no-syncopation 
                                               rule::only-simple-syncopations rule::only-simple-tuplet-offs
                                               rule::start-with-rest
                                               rule::metric-offset-of-motif rule::phrase-length
                                               rule::similar-sim-durations
                                               rule::metric-accents
                                               rule::accents-in-other-voice
                                               ) Nil)
   
   ("Melody" Nil Nil (rule::min/max-interval 
                      rule::set-pitches rule::set-intervals rule::prefer-interval-hr 
                      rule::accumulative-interval
                      rule::no-direct-repetition rule::no-repetition 
                      rule::restrict-consecutive-directions rule::resolve-skips
                      rule::durations-control-intervals
                      ) Nil)
   ("Harmony" Nil Nil (rule::only-scale-PCs 
                       rule::only-chord-PCs rule::only-spectrum-pitches 
                       rule::long-notes-chord-PCs rule::chord-tone-before/after-rest
                       rule::chord-PC-at-1st-tone-HACK rule::stepwise-non-chord-tone-resolution 
                       rule::chord-tone-follows-non-chord-tone
                       rule::unequal-sim-PCs rule::number-of-sim-PCs rule::set-harmonic-intervals 
                       rule::min/max-harmonic-interval
                       rule::tintinnabuli-M-voice rule::tintinnabuli-T-voice
                       rule::set-chord-at-positions rule::set-root-at-positions
                       rule::limit-voice-leading-distance
                       rule::schoenberg-progression-rule rule::ascending-progression 
                       rule::resolve-descending-progression
                       ) Nil)
   ("COUNTERPOINT" Nil Nil (rule::no-voice-crossing ;<== package-name-or-nickname::function-name
                            rule::no-parallels
                            ) Nil)
   ("Utilities" Nil Nil (rule::scale->pitchdomain
			 rule::file-in-this-directory 
                         rule::read-lisp-file 
                         rule::map-pairwise
                         rule::mappend
                         ) Nil)
   
   ))


