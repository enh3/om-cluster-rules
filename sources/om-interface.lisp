;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package :om-cluster-rules)

;;; ---------------------------------------------------------------------------
;;; OM / FENV bridge
;;; ---------------------------------------------------------------------------

(defun %ensure-list (x)
  (if (listp x) x (list x)))

(defun %bpf-p (x)
  (typep x 'om::bpf))

(defun %bpf-lib-p (x)
  (typep x 'om::bpf-lib))

(defun %normalise-bpf-xs (xs)
  "Map an OM BPF x-axis to FENV's [0,1] interval, preserving spacing."
  (cond ((null xs) nil)
        ((null (rest xs)) '(0))
        (t
         (let* ((x0 (first xs))
                (x1 (car (last xs)))
                (span (- x1 x0)))
           (if (zerop span)
               (loop for i from 0 below (length xs)
                     collect (/ i (max 1 (1- (length xs)))))
               (mapcar (lambda (x) (/ (- x x0) span)) xs))))))

(defun %bpf->fenv (bpf)
  "Convert an OM BPF to the internal FENV representation."
  (let ((xs (om::x-points bpf))
        (ys (om::y-points bpf)))
    (cond ((null ys) (error "Cannot convert an empty BPF to FENV."))
          ((null (rest ys)) (fenv:list->fenv ys))
          (t (fenv:list->fenv ys :xs (%normalise-bpf-xs xs))))))

(defun %fenv->bpf (env &optional (samples 100))
  "Convert an FENV to an OM BPF.  Kept as the reverse bridge for OM code."
  (let* ((n (max 2 samples))
         (xs (loop for i from 0 below n collect (* 100 (/ i (1- n)))))
         (ys (fenv:fenv->list env n)))
    (om::simple-bpf-from-list xs ys 'om::bpf 3)))

(defun %profile->fenv (profile)
  "Convert BPF/BPF-LIB profile inputs while leaving ordinary Lisp data intact."
  (cond ((%bpf-p profile) (%bpf->fenv profile))
        ((%bpf-lib-p profile)
         (mapcar #'%bpf->fenv (om::bpf-list profile)))
        ((and (listp profile) (every #'%bpf-p profile))
         (mapcar #'%bpf->fenv profile))
        (t profile)))

(defun %sample-profile (profile n)
  (cond ((%bpf-p profile) (fenv:fenv->list (%bpf->fenv profile) n))
        ((fenv:fenv? profile) (fenv:fenv->list profile n))
        ((listp profile) profile)
        (t (error "Unsupported profile: ~S" profile))))

(defun %mc-pc (pitch)
  "12-TET pitch class (0..11) from an OM midicent pitch."
  (when pitch (/ (mod pitch 1200) 100)))

(defun %in-harmony-mc? (pitches)
  "OM/midicent version of the Cluster Rules harmony-membership predicate."
  (let ((voice-pitch (first pitches))
        (harmony-pitches (second pitches)))
    (if (and voice-pitch harmony-pitches)
        (let ((harmony-pcs (mapcar #'%mc-pc harmony-pitches)))
          (every (lambda (p) (member (%mc-pc p) harmony-pcs))
                 (tu:ensure-list voice-pitch)))
        t)))

(defun %in-spectrum? (pitches)
  "True when the pitch/chord in the first voice belongs to the absolute-pitch spectrum in the second."
  (let ((voice-pitch (first pitches))
        (spectrum (second pitches)))
    (if (and voice-pitch spectrum)
        (every (lambda (p) (member p spectrum :test #'equal))
               (tu:ensure-list voice-pitch))
        t)))

(defun %pc-spec (pitch-or-pc)
  "Accept either an OM midicent pitch or a 0..11 pitch-class value."
  (if (and (numberp pitch-or-pc) (> (abs pitch-or-pc) 11))
      (%mc-pc pitch-or-pc)
      (mod pitch-or-pc 12)))

(defun %scale-list (xs new-min new-max)
  (if (null xs)
      nil
      (let ((old-min (apply #'min xs))
            (old-max (apply #'max xs)))
        (if (= old-min old-max)
            (make-list (length xs) :initial-element new-min)
            (mapcar (lambda (x)
                      (+ new-min
                         (* (/ (- x old-min) (- old-max old-min))
                            (- new-max new-min))))
                    xs)))))

(defun %random-between (lo hi)
  (if (= lo hi)
      lo
      (+ lo (random (float (- hi lo))))))

(defun %same-direction (a b)
  "True when the two intervals point the same way; zero matches nothing."
  (cond ((plusp a) (plusp b)) ((minusp a) (minusp b)) (t nil)))

(defun %rule-literal (val)
  "Form that reproduces VAL inside a rule body built by %RULE-FUNCTION."
  (if (or (symbolp val) (consp val)) `',val val))

(defmacro %rule-function ((&rest captured) lambda-list &body body)
  "Build a Cluster-Engine rule function with no lexical environment: each CAPTURED
variable is inlined as a literal, so FIX-OMPATCH-RULE can re-evaluate the body."
  `(eval `(function (lambda ,',lambda-list
                      (let ,(list ,@(loop for v in captured
                                          collect `(list ',v (%rule-literal ,v))))
                        ,@',body)))))

;;; ---------------------------------------------------------------------------
;;; PROFILE
;;; ---------------------------------------------------------------------------

(om::defmethod! om-cr::follow-timed-profile-hr
    (profile &key (voices 0) (profile-duration 1) (start 0) (end nil)
             (mode :pitch) (constrain :profile) (gracenotes? :normal)
             (interpolate-score? :no) (weight-offset 0))
  :icon 1
  :initvals '(nil 0 1 0 nil :pitch :profile :normal :no 0)
  :indoc '("profile (list, BPF, BPF-LIB or FENV)" "voices" "profile duration"
           "start" "end" "mode" "constraint" "grace notes" "interpolate" "weight offset")
  :menuins '((5 (("pitch" :pitch) ("rhythm" :rhythm)))
             (6 (("profile" :profile) ("intervals" :intervals) ("directions" :directions)))
             (7 (("normal" :normal) ("exclude-gracenotes" :exclude-gracenotes)))
             (8 (("yes" :yes) ("no" :no))))
  :doc "Heuristic rule following a time-aware profile. OM BPF/BPF-LIB inputs are converted internally to FENV."
  (rule::follow-timed-profile-hr (%profile->fenv profile)
                                 :voices voices :profile-duration profile-duration
                                 :start start :end end :mode mode :constrain constrain
                                 :gracenotes? gracenotes? :interpolate-score? interpolate-score?
                                 :weight-offset weight-offset))

(om::defmethod! om-cr::follow-profile-hr
    (profile &key (voices 0) (n 0) (mode :pitch) (constrain :profile)
             (start 0) (weight-offset 0))
  :icon 1
  :initvals '(nil 0 0 :pitch :profile 0 0)
  :indoc '("profile (list, BPF, BPF-LIB or FENV)" "voices" "number of notes"
           "mode" "constraint" "start" "weight offset")
  :menuins '((3 (("pitch" :pitch) ("rhythm" :rhythm)))
             (4 (("profile" :profile) ("intervals" :intervals) ("directions" :directions))))
  :doc "Heuristic rule following a profile. OM BPF/BPF-LIB inputs are converted internally to FENV."
  (rule::follow-profile-hr (%profile->fenv profile)
                           :voices voices :n n :mode mode :constrain constrain
                           :start start :weight-offset weight-offset))

(om::defmethod! om-cr::follow-interval-profile
    (profile &key (voices 0) (n 0) (step-size 200) (start 0))
  :icon 1
  :initvals '(nil 0 0 200 0)
  :indoc '("profile (list or BPF)" "voices" "number of notes" "step size (midicents)" "start")
  :doc "Strict melodic rule following the interval categories of a profile. This is the OM port of the public PWGL box."
  (let ((list-profile (cond ((%bpf-p profile)
                             (if (> n 0) (%sample-profile profile n)
                                 (error "FOLLOW-INTERVAL-PROFILE: N must be > 0 for a BPF.")))
                            ((fenv:fenv? profile)
                             (if (> n 0) (fenv:fenv->list profile n)
                                 (error "FOLLOW-INTERVAL-PROFILE: N must be > 0 for an FENV.")))
                            ((listp profile) profile)
                            (t (error "Unsupported profile: ~S" profile)))))
    (ce::R-pitches-one-voice
     (%rule-function (start n list-profile step-size) (xs)
       (let ((l (- (length xs) start)))
         (if (and (>= l 2) (or (= n 0) (<= l n)) (<= l (length list-profile)))
             (let ((profile-interval (- (nth (- l 1) list-profile)
                                        (nth (- l 2) list-profile)))
                   (solution-interval (- (first (last xs))
                                         (first (last xs 2)))))
               (cond ((zerop profile-interval) (zerop solution-interval))
                     ((<= (abs profile-interval) step-size)
                      (and (<= (abs solution-interval) step-size)
                           (%same-direction profile-interval solution-interval)))
                     (t (and (> (abs solution-interval) step-size)
                             (%same-direction profile-interval solution-interval)))))
             t)))
     voices :all-pitches :true/false 1)))

(om::defmethod! om-cr::rhythm-profile-bpf-hr
    (bpf &key (voices 0) (n 16) (min-scaling 1/16) (max-scaling 1)
         (rnd-deviation 0))
  :icon 1
  :initvals '(nil 0 16 1/16 1 0)
  :indoc '("BPF or BPF-LIB" "voices" "number of notes" "minimum duration"
           "maximum duration" "random deviation")
  :doc "Heuristic rhythmic profile rule, ported from the original PWGL BPF rule."
  (let* ((profiles (cond ((%bpf-lib-p bpf) (om::bpf-list bpf))
                         ((and (listp bpf) (every #'%bpf-p bpf)) bpf)
                         (t (list bpf))))
         (voice-list (%ensure-list voices)))
    (loop for profile in profiles
          for voice in voice-list
          append
          (let* ((samples (%sample-profile profile n))
                 (scaled (%scale-list (mapcar (lambda (x) (expt x 3))
                                              (%scale-list samples 0 1))
                                      min-scaling max-scaling))
                 (dev (abs rnd-deviation))
                 (target (mapcar (lambda (x)
                                   (* x (+ 1 (%random-between (- dev) dev))))
                                 scaled)))
            (ce::HR-rhythms-one-voice
             (%rule-function (target) (xs)
               (- 1000 (* 100 (abs (- (abs (first (last xs)))
                                      (abs (nth (1- (length xs)) target)))))))
             voice :all-durations)))))

(om::defmethod! om-cr::compose-functions (&rest functions)
  :icon 1
  :initvals '(nil)
  :indoc '("functions")
  :doc "Compose mapping/transformation functions from left to right."
  (apply #'rule::compose-functions functions))

(om::defmethod! om-cr::mp-add-offset ((offset number))
  :icon 1 :initvals '(0) :indoc '("offset")
  :doc "Return a profile mapping function that adds OFFSET."
  (rule::mp-add-offset offset))

(om::defmethod! om-cr::mp-multiply ((factor number))
  :icon 1 :initvals '(1) :indoc '("factor")
  :doc "Return a profile mapping function that multiplies by FACTOR."
  (rule::mp-multiply factor))

(om::defmethod! om-cr::mp-add-random-offset ((max-random-offset number))
  :icon 1 :initvals '(0) :indoc '("maximum random offset")
  :doc "Return a profile mapping function adding a random offset in +/- MAX-RANDOM-OFFSET."
  (let ((a (abs max-random-offset)))
    (lambda (x) (+ x (%random-between (- a) a)))))

(om::defmethod! om-cr::trfm-scale ((minimum number) (maximum number))
  :icon 1 :initvals '(0 1) :indoc '("minimum" "maximum")
  :doc "Return a profile transformation that scales a numeric list to MINIMUM..MAXIMUM."
  (lambda (xs) (%scale-list xs minimum maximum)))

(om::defmethod! om-cr::trfm-add-bpf ((bpf om::bpf))
  :icon 1 :initvals '(nil) :indoc '("BPF")
  :doc "Return a transformation that adds the sampled BPF to a profile list."
  (lambda (xs) (mapcar #'+ xs (%sample-profile bpf (length xs)))))

(om::defmethod! om-cr::trfm-multiply-bpf ((bpf om::bpf))
  :icon 1 :initvals '(nil) :indoc '("BPF")
  :doc "Return a transformation that multiplies a profile list by a sampled BPF."
  (lambda (xs) (mapcar #'* xs (%sample-profile bpf (length xs)))))

(om::defmethod! om-cr::trfm-reverse ()
  :icon 1 :initvals nil
  :doc "Return a transformation that reverses a profile list."
  (lambda (xs) (reverse xs)))

;;; ---------------------------------------------------------------------------
;;; RHYTHM
;;; ---------------------------------------------------------------------------

(om::defmethod! om-cr::no-two-consecutive-syncopations
    (&key (voices 0) (metric-structure :1st-beat)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 :1st-beat :true/false 1)
  :indoc '("voices" "metric structure" "rule type" "weight")
  :menuins '((1 (("beats" :beats) ("1st beat" :1st-beat)))
             (2 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "For any two consecutive beats/bars, at least one note must start on the beat."
  (rule::no-two-consecutive-syncopations :voices voices
                                         :metric-structure metric-structure
                                         :rule-type rule-type :weight weight))

(om::defmethod! om-cr::no-syncopation
    (&key (voices 0) (metric-structure :1st-beat)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 :1st-beat :true/false 1)
  :indoc '("voices" "metric structure" "rule type" "weight")
  :menuins '((1 (("beats" :beats) ("1st beat" :1st-beat)))
             (2 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Prohibits syncopation with respect to the selected metric level."
  (rule::no-syncopation :voices voices :metric-structure metric-structure
                        :rule-type rule-type :weight weight))

(om::defmethod! om-cr::no-syncopation-unless-accented
    (&key (voices 0) (metric-structure :1st-beat)
          (accent-rule :longer-than-predecessor)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 :1st-beat :longer-than-predecessor :true/false 1)
  :indoc '("voices" "metric structure" "accent rule" "rule type" "weight")
  :menuins '((1 (("beats" :beats) ("1st beat" :1st-beat)))
             (2 (("longer than predecessor" :longer-than-predecessor)
                 ("longer than predecessor (strict)" :longer-than-predecessor-strict)
                 ("longer than neighbours" :longer-than-neighbours)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Allows syncopation only when the note satisfies the selected accent rule."
  (rule::no-syncopation-unless-accented :voices voices
                                        :metric-structure metric-structure
                                        :accent-rule accent-rule
                                        :rule-type rule-type :weight weight))

(om::defmethod! om-cr::only-simple-syncopations
    (&key (voices 0) (gracenote-mode :normal)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 :normal :true/false 1)
  :indoc '("voices" "grace notes" "rule type" "weight")
  :menuins '((1 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (2 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Restricts syncopations over beats to relatively simple cases."
  (rule::only-simple-syncopations :voices voices :gracenote-mode gracenote-mode
                                  :rule-type rule-type :weight weight))

(om::defmethod! om-cr::only-simple-tuplet-offs
    (&key (voices 0) (gracenote-mode :normal)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 :normal :true/false 1)
  :indoc '("voices" "grace notes" "rule type" "weight")
  :menuins '((1 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (2 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Restricts rhythmic positions of tuplets to relatively simple cases."
  (rule::only-simple-tuplet-offs :voices voices :gracenote-mode gracenote-mode
                                 :rule-type rule-type :weight weight))

(om::defmethod! om-cr::start-with-rest
    (&key (rest-dur 0) (voices 0) (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 0 :true/false 1)
  :indoc '("rest duration (or domain)" "voices" "rule type" "weight")
  :menuins '((2 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Starts the given voice(s) with a rest of the specified duration. NIL accepts any rest duration."
  (rule::start-with-rest :rest-dur rest-dur :voices voices
                         :rule-type rule-type :weight weight))

(om::defmethod! om-cr::metric-offset-of-motif
    (&key (metric-offset 0) (voices 0) (metric-structure :1st-beat)
          (grid 1/4) (min-motif-length nil)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 0 :1st-beat 1/4 nil :true/false 1)
  :indoc '("metric offset" "voices" "metric structure" "grid"
           "minimum motif length" "rule type" "weight")
  :menuins '((2 (("beats" :beats) ("1st beat" :1st-beat)))
             (5 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Constrains motif beginnings to a given metric offset."
  (rule::metric-offset-of-motif :metric-offset metric-offset :voices voices
                                :metric-structure metric-structure :grid grid
                                :min-motif-length min-motif-length
                                :rule-type rule-type :weight weight))

(om::defmethod! om-cr::phrase-length
    (phrase-length &key (voices 0) (relation :min) (n 32)
                   (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(4 0 :min 32 :true/false 1)
  :indoc '("phrase length (number/list/BPF/FENV)" "voices" "relation"
           "number of profile samples" "rule type" "weight")
  :menuins '((2 (("minimum" :min) ("maximum" :max)))
             (4 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Controls the number of notes/grace notes between rests. A BPF can define a changing phrase length profile."
  (let* ((profile? (or (%bpf-p phrase-length) (fenv:fenv? phrase-length)))
         (vals (and profile? (%sample-profile phrase-length n))))
    (ce::R-rhythms-one-voice
     (%rule-function (n profile? vals phrase-length relation) (durs)
       (let* ((idx (min (max 0 (1- (length durs))) (max 0 (1- n))))
              (current (if profile? (nth idx vals) phrase-length))
              (rev (reverse durs)))
         (if (second rev)
             (case relation
               (:min
                (if (and (rule::is-rest? (first rev))
                         (not (rule::is-rest? (second rev))))
                    (let ((prev-rest-pos (position-if #'rule::is-rest? (rest rev))))
                      (if prev-rest-pos
                          (<= current prev-rest-pos)
                          (<= current (length rev))))
                    t))
               (:max
                (if (not (rule::is-rest? (first rev)))
                    (let ((prev-rest-pos (position-if #'rule::is-rest? (rest rev))))
                      (if prev-rest-pos
                          (>= (1- current) prev-rest-pos)
                          (>= current (length rev))))
                    t))
               (otherwise (error "Unknown phrase-length relation: ~S" relation)))
             t)))
     voices :all-durations rule-type weight)))

(om::defmethod! om-cr::similar-sim-durations
    (&key (voices '(0 1)) (max-factor 1) (rest-mode :constrain)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '((0 1) 1 :constrain :true/false 1)
  :indoc '("voices" "maximum duration factor" "rest mode" "rule type" "weight")
  :menuins '((2 (("constrain rests" :constrain) ("ignore rests" :ignore)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Restricts the maximum difference between simultaneous note durations."
  (rule::similar-sim-durations :voices voices :max-factor max-factor
                               :rest-mode rest-mode :rule-type rule-type :weight weight))

(om::defmethod! om-cr::metric-accents
    (&key (voices 0) (metric-structure :1st-beat)
          (accent-rule :longer-than-predecessor) (strictness :note)
          (format :d_offs) (gracenote-mode :normal)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 :1st-beat :longer-than-predecessor :note :d_offs :normal :true/false 1)
  :indoc '("voices" "metric structure" "accent rule" "strictness" "format"
           "grace notes" "rule type" "weight")
  :menuins '((1 (("beats" :beats) ("1st beat" :1st-beat)))
             (2 (("longer than predecessor" :longer-than-predecessor)
                 ("longer than predecessor (strict)" :longer-than-predecessor-strict)
                 ("longer than neighbours" :longer-than-neighbours)))
             (3 (("note -> position" :note) ("position -> note" :position)
                 ("note <-> position" :note-n-position)))
             (4 (("duration/offset" :d_offs) ("duration/offset/motif" :d_offs_m)
                 ("duration/offset/motif/index" :d_offs_m_n)))
             (5 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (6 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Constrains rhythmic accents in relation to the underlying meter."
  (rule::metric-accents :voices voices :metric-structure metric-structure
                        :accent-rule accent-rule :strictness strictness :format format
                        :gracenote-mode gracenote-mode :rule-type rule-type :weight weight))

(om::defmethod! om-cr::accents-in-other-voice
    (&key (voices 1) (accents-voice 0)
          (accent-rule :longer-than-predecessor) (strictness :note)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(1 0 :longer-than-predecessor :note :true/false 1)
  :indoc '("voices" "accent voice" "accent rule" "strictness" "rule type" "weight")
  :menuins '((2 (("longer than predecessor" :longer-than-predecessor)
                 ("longer than predecessor (strict)" :longer-than-predecessor-strict)
                 ("longer than neighbours" :longer-than-neighbours)))
             (3 (("note -> position" :note) ("position -> note" :position)
                 ("note <-> position" :note-n-position)))
             (4 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Constrains accents in VOICES according to note onsets in ACCENTS-VOICE."
  (rule::accents-in-other-voice :voices voices :accents-voice accents-voice
                                :accent-rule accent-rule :strictness strictness
                                :rule-type rule-type :weight weight))

(om::defmethod! om-cr::mk-accent-has-at-least-duration-ar
    (&key (min-duration 1/4))
  :icon 1 :initvals '(1/4) :indoc '("minimum duration")
  :doc "Returns an accent-rule function requiring at least MIN-DURATION."
  (rule::mk-accent-has-at-least-duration-ar :min-duration min-duration))

(om::defmethod! om-cr::mk-accent->-prep-or->=-dur-ar
    (&key (min-duration 1/4))
  :icon 1 :initvals '(1/4) :indoc '("minimum duration")
  :doc "Returns an accent rule: longer than predecessor OR at least MIN-DURATION."
  (rule::mk-accent->-prep-OR->=-dur-ar :min-duration min-duration))

(om::defmethod! om-cr::mk-accent->-prep-and->=-dur-ar
    (&key (duration-threshold 1/4))
  :icon 1 :initvals '(1/4) :indoc '("duration threshold")
  :doc "Returns an accent rule: longer than predecessor AND at least DURATION-THRESHOLD."
  (rule::mk-accent->-prep-AND->=-dur-ar :duration-threshold duration-threshold))

(om::defmethod! om-cr::thomassen-accents ((midi-pitches list))
  :icon 1 :initvals '(nil) :indoc '("MIDI pitches")
  :doc "Returns Thomassen melodic accent strengths for a MIDI pitch sequence."
  (rule::thomassen-accents midi-pitches))

(om::defmethod! om-cr::thomassen-accents-ar
    (&key (thomassen-accent-strength 0.4))
  :icon 1 :initvals '(0.4) :indoc '("accent strength threshold")
  :doc "Returns a Thomassen accent-rule function."
  (rule::thomassen-accents-ar thomassen-accent-strength))

;;; ---------------------------------------------------------------------------
;;; MELODY
;;; ---------------------------------------------------------------------------

(om::defmethod! om-cr::min/max-interval
    (&key (voices 0) (min-interval nil) (max-interval nil) (n 0)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 nil nil 0 :true/false 1)
  :indoc '("voices" "minimum interval (midicents or BPF)" "maximum interval (midicents or BPF)"
           "number of notes" "rule type" "weight")
  :menuins '((4 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Limit melodic interval size. In OM, interval values are expressed in midicents. BPF inputs are converted to FENV internally."
  (rule::min/max-interval :voices voices
                          :min-interval (%profile->fenv min-interval)
                          :max-interval (%profile->fenv max-interval)
                          :n n :rule-type rule-type :weight weight))

(om::defmethod! om-cr::set-pitches
    (pitches &key (voices 0) (pcs? :pitches) (mode :only-given)
             (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(nil 0 :pitches :only-given :true/false 1)
  :indoc '("pitches or pitch classes" "voices" "pitch mode" "include/exclude" "rule type" "weight")
  :menuins '((2 (("pitches" :pitches) ("pitch classes" :pcs)))
             (3 (("only given" :only-given) ("exclude given" :exclude-given)))
             (4 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Restrict pitches or pitch classes. Absolute pitches are OM midicents; pitch classes are integers 0..11."
  (if (eq pcs? :pitches)
      (rule::set-pitches :voices voices :pitches pitches :pcs? :pitches
                         :mode mode :rule-type rule-type :weight weight)
      (let ((pcs (mapcar #'%pc-spec pitches)))
        (ce::R-pitches-one-voice
         (%rule-function (pcs mode) (pitch)
           (if pitch
               (let ((member? (member (%mc-pc pitch) pcs)))
                 (case mode
                   (:only-given member?)
                   (:exclude-given (not member?))))
               t))
         voices :pitches rule-type weight))))

(om::defmethod! om-cr::set-intervals
    (intervals &key (absolute? :absolute) (mode :only-given) (voices 0)
               (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(nil :absolute :only-given 0 :true/false 1)
  :indoc '("intervals (midicents)" "direction mode" "include/exclude" "voices" "rule type" "weight")
  :menuins '((1 (("absolute" :absolute) ("up/down" :up/down)))
             (2 (("only given" :only-given) ("exclude given" :exclude-given)))
             (4 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Restrict melodic intervals. In OM interval values are expressed in midicents."
  (rule::set-intervals :intervals intervals :absolute? absolute? :mode mode
                       :voices voices :rule-type rule-type :weight weight))

(om::defmethod! om-cr::prefer-interval-hr
    (interval &key (voices 0) (n 0) (weight-factor 1))
  :icon 1
  :initvals '(100 0 0 1)
  :indoc '("preferred interval (midicents or BPF)" "voices" "number of notes" "weight factor")
  :doc "Heuristic preference for a melodic interval. OM interval values are midicents; a BPF/FENV can vary the preferred interval over time."
  (let* ((profile? (or (%bpf-p interval) (fenv:fenv? interval)))
         (offsets (when profile?
                    (if (> n 0)
                        (%sample-profile interval n)
                        (error "PREFER-INTERVAL-HR: N must be > 0 when INTERVAL is a BPF/FENV.")))))
    (ce::HR-pitches-one-voice
     (cond ((and (numberp interval) (= n 0))
            (%rule-function (interval weight-factor) (p1 p2)
              (if (and p1 p2)
                  (- (* (abs (- interval (abs (- p1 p2)))) weight-factor))
                  0)))
           (t
            (let ((vals (or offsets (make-list n :initial-element interval))))
              (%rule-function (vals weight-factor) (p1n p2n)
                (destructuring-bind (p2 index) p2n
                  (if (and p2 (< index (length vals)))
                      (- (* (abs (- (nth index vals)
                                    (abs (- (first p1n) p2))))
                            weight-factor))
                      0))))))
     voices (if (= n 0) :pitches :pitch/nth))))

(om::defmethod! om-cr::accumulative-interval
    (n interval &key (condition :max) (sublists :ignore) (number-of-notes 0)
                (voices 0) (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(2 700 :max :ignore 0 0 :true/false 1)
  :indoc '("number of notes" "interval (midicents or BPF)" "condition" "constrain sublists"
           "total number of notes" "voices" "rule type" "weight")
  :menuins '((2 (("maximum" :max) ("minimum" :min) ("equal" :equal)))
             (3 (("constrain" :constrain) ("ignore" :ignore)))
             (6 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Constrains the accumulated melodic span over N notes. OM interval values are midicents."
  (let* ((profile? (or (%bpf-p interval) (fenv:fenv? interval)))
         (intervals (when profile?
                      (if (> number-of-notes 0)
                          (%sample-profile interval number-of-notes)
                          (error "ACCUMULATIVE-INTERVAL: NUMBER-OF-NOTES must be > 0 for BPF/FENV.")))))
    (labels ((make-rule (count)
               (ce::R-pitches-one-voice
                (%rule-function (count profile? intervals interval condition) (pitches)
                  (let ((ps (last pitches count)))
                    (if (and (= (length ps) count) (every #'identity ps))
                        (let* ((limit (if profile?
                                          (nth (min (1- (length pitches)) (1- (length intervals))) intervals)
                                          interval))
                               (span (abs (- (car (last ps)) (first ps)))))
                          (funcall (case condition (:max #'<=) (:min #'>=) (:equal #'=)) span limit))
                        t)))
                voices :all-pitches rule-type weight)))
      (if (and (eq sublists :constrain) (eq condition :max))
          (loop for count from 2 to n append (make-rule count))
          (make-rule n)))))

(om::defmethod! om-cr::no-direct-repetition
    (&key (voices 0) (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 :true/false 1)
  :indoc '("voices" "rule type" "weight")
  :menuins '((1 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Disallow direct pitch/chord repetition."
  (ce::R-pitches-one-voice
   (%rule-function () (p1 p2)
     (if (and p1 p2)
         (not (equal p1 p2))
         t))
   voices :pitches rule-type weight))

(om::defmethod! om-cr::no-repetition
    (&key (voices 0) (window 2) (mode :pitches)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 2 :pitches :true/false 1)
  :indoc '("voices" "window" "mode" "rule type" "weight")
  :menuins '((2 (("pitches" :pitches) ("pitch classes" :pcs)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Disallow repetitions within a melodic window. Pitch-class mode is adapted to OM midicents."
  (ce::R-pitches-one-voice
   (%rule-function (mode window) (pitches)
     (let* ((ps (mapcar (lambda (p)
                          (if (and p (eq mode :pcs)) (%mc-pc p) p))
                        (last pitches window)))
            (p1 (car (last ps))))
       (if p1 (not (member p1 (butlast ps) :test #'equal)) t)))
   voices :all-pitches rule-type weight))

(om::defmethod! om-cr::durations-control-intervals
    (&key (voices 0) (rel-factor 3200) (acc-factor 2)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 3200 2 :true/false 1)
  :indoc '("voices" "relation factor" "accuracy factor" "rule type" "weight")
  :menuins '((3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Relates note durations and melodic interval sizes. Default relation factor is scaled for OM midicents."
  (rule::durations-control-intervals :voices voices :rel-factor rel-factor
                                     :acc-factor acc-factor :rule-type rule-type :weight weight))

(om::defmethod! om-cr::restrict-consecutive-directions
    (&key (n 4) (direction :either) (voices 0)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(4 :either 0 :true/false 1)
  :indoc '("number of intervals" "direction" "voices" "rule type" "weight")
  :menuins '((1 (("either" :either) ("either strict" :either-strict)
                 ("ascending" :ascending) ("ascending strict" :ascending-strict)
                 ("descending" :descending) ("descending strict" :descending-strict)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Restrict the number of consecutive melodic intervals moving in the same direction."
  (rule::restrict-consecutive-directions :n n :direction direction :voices voices
                                         :rule-type rule-type :weight weight))

(om::defmethod! om-cr::resolve-skips
    (&key (skip-size 600) (resolution-size 200) (repetition? :disallow)
          (voices 0) (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(600 200 :disallow 0 :true/false 1)
  :indoc '("skip size (midicents)" "resolution size (midicents)" "repetition" "voices" "rule type" "weight")
  :menuins '((2 (("disallow" :disallow) ("allow" :allow)))
             (4 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Resolve melodic skips in the opposite direction. OM interval values are midicents."
  (rule::resolve-skips :skip-size skip-size :resolution-size resolution-size
                       :repetition? repetition? :voices voices
                       :rule-type rule-type :weight weight))

;;; ---------------------------------------------------------------------------
;;; HARMONY
;;; ---------------------------------------------------------------------------

(defun %pc-member-mc (pitch pitches)
  (and pitch (member (%mc-pc pitch) (mapcar #'%mc-pc (remove nil pitches)))))

(defun %mc-chord->semitones (chord)
  (mapcar (lambda (p) (if p (/ p 100) p)) chord))

(om::defmethod! om-cr::only-scale-pcs
    (&key (voices 2) (input-mode :all) (gracenotes? :normal)
          (rule-type :true/false) (weight 1) (scale-voice 0))
  :icon 1
  :initvals '(2 :all :normal :true/false 1 0)
  :indoc '("voices" "input mode" "grace notes" "rule type" "weight" "scale voice")
  :menuins '((1 (("all" :all) ("beat" :beat) ("1st beat" :1st-beat) ("1st voice" :1st-voice)))
             (2 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Constrain tones to the pitch classes of the simultaneous scale voice."
  (loop for voice in (%ensure-list voices) append
        (ce::R-pitch-pitch #'%in-harmony-mc? (list voice scale-voice) '(0)
                           input-mode gracenotes? :pitch rule-type weight)))

(om::defmethod! om-cr::only-chord-pcs
    (&key (voices 2) (input-mode :all) (gracenotes? :normal)
          (rule-type :true/false) (weight 1) (chord-voice 1))
  :icon 1
  :initvals '(2 :all :normal :true/false 1 1)
  :indoc '("voices" "input mode" "grace notes" "rule type" "weight" "chord voice")
  :menuins '((1 (("all" :all) ("beat" :beat) ("1st beat" :1st-beat) ("1st voice" :1st-voice)))
             (2 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Constrain tones to the pitch classes of the simultaneous chord voice."
  (loop for voice in (%ensure-list voices) append
        (ce::R-pitch-pitch #'%in-harmony-mc? (list voice chord-voice) '(0)
                           input-mode gracenotes? :pitch rule-type weight)))

(om::defmethod! om-cr::only-spectrum-pitches
    (&key (voices 2) (input-mode :all) (gracenotes? :normal)
          (rule-type :true/false) (weight 1) (chord-voice 1))
  :icon 1
  :initvals '(2 :all :normal :true/false 1 1)
  :indoc '("voices" "input mode" "grace notes" "rule type" "weight" "spectrum voice")
  :menuins '((1 (("all" :all) ("beat" :beat) ("1st beat" :1st-beat) ("1st voice" :1st-voice)))
             (2 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Constrain absolute pitches to the simultaneous pitches of another voice."
  (loop for voice in (%ensure-list voices) append
        (ce::R-pitch-pitch #'%in-spectrum? (list voice chord-voice) '(0)
                           input-mode gracenotes? :pitch rule-type weight)))

(om::defmethod! om-cr::long-notes-chord-pcs
    (max-nonharmonic-dur &key (voices 2) (gracenotes? :normal)
         (rule-type :true/false) (weight 1) (chord-voice 1))
  :icon 1
  :initvals '(1 2 :normal :true/false 1 1)
  :indoc '("maximum non-harmonic duration" "voices" "grace notes" "rule type" "weight" "chord voice")
  :menuins '((2 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Long notes must be chord tones. Pitch-class comparison is adapted to OM midicents."
  (let ((rule
          (%rule-function (max-nonharmonic-dur) (p-d-offs)
            (destructuring-bind ((pitch1 dur1 offs1)
                                 (pitch2 dur2 offs2))
                p-d-offs
              (declare (ignore offs1 dur2 offs2))
              (if (and pitch1 pitch2 (> dur1 max-nonharmonic-dur))
                  (member (mod pitch1 1200)
                          (mapcar (lambda (p) (mod p 1200)) pitch2))
                  t)))))
    (loop for voice in (%ensure-list voices) append
          (ce::R-pitch-pitch rule (list voice chord-voice) '(0)
                             :all gracenotes? :p_d_offs rule-type weight))))

(om::defmethod! om-cr::chord-tone-before/after-rest
    (&key (voices 2) (input-mode :all) (gracenotes? :normal)
          (rule-type :true/false) (weight 1) (chord-voice 1))
  :icon 1
  :initvals '(2 :all :normal :true/false 1 1)
  :indoc '("voices" "input mode" "grace notes" "rule type" "weight" "chord voice")
  :menuins '((1 (("all" :all) ("beat" :beat) ("1st beat" :1st-beat)))
             (2 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Notes immediately before/after rests must be chord tones."
  (loop for voice in (%ensure-list voices) append
        (ce::R-pitch-pitch
         (%rule-function () (pitches1 pitches2)
           (let ((p1 (first pitches1)) (p2 (first pitches2)))
             (cond ((and (null p1) (null p2)) t)
                   ((null p1) (%pc-member-mc p2 (second pitches2)))
                   ((null p2) (%pc-member-mc p1 (second pitches1)))
                   (t t))))
         (list voice chord-voice) '(0) input-mode gracenotes? :pitch rule-type weight)))

(om::defmethod! om-cr::chord-pc-at-1st-tone-hack
    (chords &key (voices 2) (input-mode :position-for-pitches)
            (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(nil 2 :position-for-pitches :true/false 1)
  :indoc '("chords" "voices" "input mode" "rule type" "weight")
  :menuins '((2 (("position for pitches" :position-for-pitches) ("index for cell" :index-for-cell)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Require the first tone to belong to the pitch classes of the first supplied chord."
  (loop for voice in (%ensure-list voices) append
        (ce::R-index-pitches-one-voice
         (%rule-function (chords) (pitch)
           (if pitch
               (member (%mc-pc pitch) (mapcar #'%mc-pc (first chords)))
               t))
         '(0) voice input-mode rule-type weight)))

(om::defmethod! om-cr::stepwise-non-chord-tone-resolution
    (&key (voices 2) (step-size 200) (input-mode :all) (gracenotes? :normal)
          (rule-type :true/false) (weight 1) (chord-voice 1))
  :icon 1
  :initvals '(2 200 :all :normal :true/false 1 1)
  :indoc '("voices" "step size (midicents)" "input mode" "grace notes" "rule type" "weight" "chord voice")
  :menuins '((2 (("all" :all) ("beat" :beat) ("1st beat" :1st-beat)))
             (3 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (4 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Non-chord tones must be reached and left by step."
  (loop for voice in (%ensure-list voices) append
        (ce::R-pitch-pitch
         (%rule-function (step-size) (p1 p2 p3)
           (if (and (first p2) (second p2))
               (let ((vp2 (second p2)))
                 (if (not (%pc-member-mc vp2 (first p2)))
                     (and (or (null (second p1)) (<= (abs (- (second p1) vp2)) step-size))
                          (or (null (second p3)) (<= (abs (- vp2 (second p3))) step-size)))
                     t))
               t))
         (list chord-voice voice) '(0) input-mode gracenotes? :pitch rule-type weight)))

(om::defmethod! om-cr::chord-tone-follows-non-chord-tone
    (&key (voices 2) (input-mode :all) (gracenotes? :normal)
          (rule-type :true/false) (weight 1) (chord-voice 1))
  :icon 1
  :initvals '(2 :all :normal :true/false 1 1)
  :indoc '("voices" "input mode" "grace notes" "rule type" "weight" "chord voice")
  :menuins '((1 (("all" :all) ("beat" :beat) ("1st beat" :1st-beat)))
             (2 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Every non-chord tone must be followed by a chord tone."
  (loop for voice in (%ensure-list voices) append
        (ce::R-pitch-pitch
         (%rule-function () (p1 p2)
           (if (and (first p1) (first p2) (second p1) (second p2))
               (if (not (%pc-member-mc (second p1) (first p1)))
                   (%pc-member-mc (second p2) (first p2))
                   t)
               t))
         (list chord-voice voice) '(0) input-mode gracenotes? :pitch rule-type weight)))

(om::defmethod! om-cr::unequal-sim-pcs
    (&key (voices '(0 1)) (input-mode :all) (gracenotes? :normal)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '((0 1) :all :normal :true/false 1)
  :indoc '("voices" "input mode" "grace notes" "rule type" "weight")
  :menuins '((1 (("all" :all) ("beat" :beat) ("1st beat" :1st-beat) ("1st voice" :1st-voice)))
             (2 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Simultaneous pitch classes in the selected voices must all be unequal."
  (ce::R-pitch-pitch
   (%rule-function () (pitches)
     (let ((pcs (mapcar #'%mc-pc (remove nil pitches))))
       (= (length pcs) (length (remove-duplicates pcs)))))
   voices '(0) input-mode gracenotes? :pitch rule-type weight))

(om::defmethod! om-cr::number-of-sim-pcs
    (&key (pc-number 2) (condition :min) (rests-mode :reduce-no)
          (voices '(0 1)) (timepoints '(0)) (input-mode :all)
          (gracenotes? :normal) (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(2 :min :reduce-no (0 1) (0) :all :normal :true/false 1)
  :indoc '("PC number" "condition" "rests mode" "voices" "timepoints" "input mode" "grace notes" "rule type" "weight")
  :menuins '((1 (("minimum" :min) ("equal" :equal) ("maximum" :max)))
             (2 (("reduce number" :reduce-no) ("ignore rests" :ignore)))
             (5 (("all" :all) ("beat" :beat) ("1st beat" :1st-beat) ("1st voice" :1st-voice) ("at timepoints" :at-timepoints)))
             (6 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (7 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Control the number of simultaneous pitch classes."
  (ce::R-pitch-pitch
   (%rule-function (rests-mode pc-number condition) (pitches)
     (let* ((nonrests (remove nil pitches))
            (target (case rests-mode (:reduce-no (- pc-number (- (length pitches) (length nonrests)))) (:ignore pc-number)))
            (pcs (remove-duplicates (mapcar #'%mc-pc nonrests))))
       (if pcs
           (funcall (case condition (:min #'>=) (:equal #'=) (:max #'<=)) (length pcs) target)
           t)))
   voices timepoints input-mode gracenotes? :pitch rule-type weight))

(om::defmethod! om-cr::set-harmonic-intervals
    (intervals &key (voices '(0 1)) (pcs? :pitches) (exclude/only :only-given)
               (combinations :over-bass) (input-mode :beat) (gracenotes? :normal)
               (timepoints '(0)) (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(nil (0 1) :pitches :only-given :over-bass :beat :normal (0) :true/false 1)
  :indoc '("intervals" "voices" "interval mode" "include/exclude" "voice combinations" "input mode" "grace notes" "timepoints" "rule type" "weight")
  :menuins '((2 (("absolute pitches" :pitches) ("pitch classes" :pcs)))
             (3 (("only given" :only-given) ("exclude given" :exclude-given)))
             (4 (("over bass" :over-bass) ("consecutive voices" :consecutive-voices) ("all combinations" :all-combinations)))
             (5 (("beat" :beat) ("all" :all) ("1st beat" :1st-beat) ("1st voice" :1st-voice) ("at timepoints" :at-timepoints)))
             (6 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (8 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Restrict harmonic intervals. Absolute intervals are midicents; PC intervals are 0..11."
  (labels ((mk (v1 v2)
             (ce::R-pitch-pitch
              (%rule-function (pcs? intervals exclude/only) (pitches)
                (if (and (first pitches) (second pitches))
                    (let* ((raw (abs (- (first pitches) (second pitches))))
                           (int (if (eq pcs? :pcs) (mod (round raw 100) 12) raw))
                           (member? (member int intervals)))
                      (if (eq exclude/only :only-given) member? (not member?)))
                    t))
              (list v1 v2) timepoints input-mode gracenotes? :pitch rule-type weight)))
    (let ((vs (sort (copy-list voices) #'>)))
      (case combinations
        (:over-bass (loop with bass = (first vs) for v in (rest vs) append (mk bass v)))
        (:consecutive-voices (loop for a in vs for b in (rest vs) append (mk a b)))
        (:all-combinations (loop for tail on vs append
                                 (loop for b in (rest tail) append (mk (first tail) b))))))))

(om::defmethod! om-cr::min/max-harmonic-interval
    (&key (voices '(0 1)) (min-interval nil) (max-interval nil)
          (input-mode :beat) (combinations :consecutive-voices)
          (gracenotes? :normal) (timepoints '(0))
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '((0 1) nil nil :beat :consecutive-voices :normal (0) :true/false 1)
  :indoc '("voices" "minimum interval (midicents)" "maximum interval (midicents)" "input mode" "voice combinations" "grace notes" "timepoints" "rule type" "weight")
  :menuins '((3 (("beat" :beat) ("all" :all) ("1st beat" :1st-beat) ("1st voice" :1st-voice) ("at timepoints" :at-timepoints)))
             (4 (("over bass" :over-bass) ("consecutive voices" :consecutive-voices) ("all combinations" :all-combinations)))
             (5 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (7 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Limit harmonic interval size in OM midicents."
  (rule::min/max-harmonic-interval :voices voices :min-interval min-interval :max-interval max-interval
                                   :input-mode input-mode :combinations combinations
                                   :gracenotes? gracenotes? :timepoints timepoints
                                   :rule-type rule-type :weight weight))

(om::defmethod! om-cr::tintinnabuli-m-voice
    (&key (voices 0) (max-interval 200) (rule-type :true/false) (weight 1) (scale-voice 0))
  :icon 1 :initvals '(0 200 :true/false 1 0)
  :indoc '("voices" "maximum melodic interval (midicents)" "rule type" "weight" "scale voice")
  :menuins '((2 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Tintinnabuli M-voice rule adapted to OM midicents."
  (ce::rules->cluster
   (om-cr::min/max-interval :voices voices :max-interval max-interval :rule-type rule-type :weight weight)
   (om-cr::only-scale-pcs :voices voices :input-mode :all :rule-type rule-type :weight weight :scale-voice scale-voice)))

(om::defmethod! om-cr::tintinnabuli-t-voice
    (&key (voices 0) (min-interval 0) (max-interval 1200)
          (rule-type :true/false) (weight 1) (chord-voice 1))
  :icon 1 :initvals '(0 0 1200 :true/false 1 1)
  :indoc '("voices" "minimum melodic interval (midicents)" "maximum melodic interval (midicents)" "rule type" "weight" "chord voice")
  :menuins '((3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Tintinnabuli T-voice rule adapted to OM midicents."
  (ce::rules->cluster
   (om-cr::min/max-interval :voices voices :min-interval min-interval :max-interval max-interval :rule-type rule-type :weight weight)
   (om-cr::only-chord-pcs :voices voices :input-mode :all :rule-type rule-type :weight weight :chord-voice chord-voice)))

(om::defmethod! om-cr::set-chord-at-positions
    (positions chord &key (chord-voice 1) (rule-type :true/false) (weight 1))
  :icon 1 :initvals '(nil nil 1 :true/false 1)
  :indoc '("positions" "chord" "chord voice" "rule type" "weight")
  :menuins '((3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Set the chord at selected positions of the chord voice."
  (rule::set-chord-at-positions positions chord :chord-voice chord-voice :rule-type rule-type :weight weight))

(om::defmethod! om-cr::set-root-at-positions
    (positions root &key (pc? t) (chord-voice 1) (rule-type :true/false) (weight 1))
  :icon 1 :initvals '(nil 6000 t 1 :true/false 1)
  :indoc '("positions" "root (midicent or PC)" "compare pitch class" "chord voice" "rule type" "weight")
  :menuins '((2 (("pitch class" t) ("absolute pitch" nil)))
             (4 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Set roots at selected chord positions. Pitch-class comparison is adapted to OM midicents."
  (let ((root-pc (%pc-spec root)))
    (loop for pos in positions append
          (ce::R-index-pitches-one-voice
           (%rule-function (pc? root-pc root) (ps)
             (if ps
                 (if pc? (= (%mc-pc (first ps)) root-pc) (= (first ps) root))
                 t))
           (list pos) chord-voice :position-for-pitches rule-type weight))))

(om::defmethod! om-cr::limit-voice-leading-distance
    (max-distance &key (chord-voice 1) (n nil) (rule-type :true/false) (weight 1))
  :icon 1 :initvals '(700 1 nil :true/false 1)
  :indoc '("maximum distance (midicents)" "chord voice" "number of PCs" "rule type" "weight")
  :menuins '((3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Limit voice-leading distance between successive chords; OM distance is expressed in midicents."
  (ce::R-pitches-one-voice
   (%rule-function (n max-distance) (c1 c2)
     (if (and c1 c2)
         (<= (* 100 (rule::voice-leading-distance (%mc-chord->semitones c1) (%mc-chord->semitones c2) n)) max-distance)
         t))
   chord-voice :pitches rule-type weight))

(om::defmethod! om-cr::ascending-progression
    (&key (chord-voice 1) (n nil) (rule-type :heur-switch) (weight 1))
  :icon 1 :initvals '(1 nil :heur-switch 1)
  :indoc '("chord voice" "number of PCs" "rule type" "weight")
  :menuins '((2 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Constrain successive chords to ascending Schoenbergian progressions."
  (ce::R-pitches-one-voice
   (%rule-function (n) (c1 c2)
     (rule::ascending-progression? (%mc-chord->semitones c1) (%mc-chord->semitones c2) n))
   chord-voice :pitches rule-type weight))

(om::defmethod! om-cr::resolve-descending-progression
    (&key (allow-repetition nil) (allow-interchange-progression nil)
          (chord-voice 1) (n nil) (rule-type :true/false) (weight 1))
  :icon 1 :initvals '(nil nil 1 nil :true/false 1)
  :indoc '("allow repetition" "allow interchange progression" "chord voice" "number of PCs" "rule type" "weight")
  :menuins '((0 (("yes" t) ("no" nil))) (1 (("yes" t) ("no" nil)))
             (4 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Resolve descending Schoenbergian chord progressions."
  (let ((rules
          (list
           (ce::R-pitches-one-voice
            (%rule-function (n allow-interchange-progression) (c1 c2 c3)
              (let ((a (%mc-chord->semitones c1)) (b (%mc-chord->semitones c2)) (c (%mc-chord->semitones c3)))
                (if (rule::descending-progression? a b n)
                    (if allow-interchange-progression
                        (or (rule::ascending-progression? a c n) (rule::constant-progression? a c))
                        (rule::ascending-progression? a c n))
                    t)))
            chord-voice :pitches rule-type weight))))
    (if allow-repetition
        rules
        (append rules
                (list (ce::R-pitches-one-voice
                       (%rule-function () (c1 c2)
                         (not (rule::constant-progression? (%mc-chord->semitones c1) (%mc-chord->semitones c2))))
                       chord-voice :pitches rule-type weight))))))

(om::defmethod! om-cr::schoenberg-progression-rule
    (&key (progression :resolve-descending-progression) (chord-voice 1) (n nil)
          (rule-type :heur-switch) (weight 1))
  :icon 1 :initvals '(:resolve-descending-progression 1 nil :heur-switch 1)
  :indoc '("progression" "chord voice" "number of PCs" "rule type" "weight")
  :menuins '((0 (("ascending" :ascending)
                 ("resolve descending" :resolve-descending-progression)
                 ("harmonic band/common PCs" :common-pcs)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Convenience wrapper for Schoenbergian progression rules."
  (case progression
    (:ascending (om-cr::ascending-progression :chord-voice chord-voice :n n :rule-type rule-type :weight weight))
    (:resolve-descending-progression
     (om-cr::resolve-descending-progression :chord-voice chord-voice :n n :rule-type rule-type :weight weight))
    ((:harmonic-band :common-pcs)
     (ce::R-pitches-one-voice
      (%rule-function (n) (c1 c2)
        (rule::common-pcs? (%mc-chord->semitones c1) (%mc-chord->semitones c2) n))
      chord-voice :pitches rule-type weight))))

;;; ---------------------------------------------------------------------------
;;; COUNTERPOINT
;;; ---------------------------------------------------------------------------

(om::defmethod! om-cr::no-voice-crossing
    (&key (voices '(0 1)) (input-mode :all) (gracenotes? :normal)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '((0 1) :all :normal :true/false 1)
  :indoc '("voices" "input mode" "grace notes" "rule type" "weight")
  :menuins '((1 (("all" :all) ("beat" :beat) ("1st beat" :1st-beat) ("1st voice" :1st-voice)))
             (2 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Prevent voice crossing."
  (let* ((vs (sort (copy-list (%ensure-list voices)) #'<))
         (rule (%rule-function () (pitches)
                 (apply #'>= (remove nil pitches)))))
    (loop for a in vs
          for b in (rest vs)
          append (ce::R-pitch-pitch rule (list a b) '(0)
                                    input-mode gracenotes? :pitch
                                    rule-type weight))))

(om::defmethod! om-cr::no-parallels
    (intervals &key (mode :open) (voices '(0 1)) (gracenotes? :normal)
               (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '((0 700) :open (0 1) :normal :true/false 1)
  :indoc '("intervals (midicents modulo octave)" "mode" "voices" "grace notes" "rule type" "weight")
  :menuins '((1 (("open" :open) ("open and hidden" :open-and-hidden)))
             (3 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (4 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Prohibit parallel harmonic intervals. OM interval values are midicents."
  (let* ((vs (%ensure-list voices))
         (rule
           (%rule-function (intervals mode) (pitches1 pitches2)
             (if (every #'identity (append pitches1 pitches2))
                 (let* ((pitch1a (first pitches1))
                        (pitch1b (second pitches1))
                        (pitch2a (first pitches2))
                        (pitch2b (second pitches2))
                        (harm-interval2
                          (mod (abs (- pitch2b pitch2a)) 1200))
                        (matching-interval2?
                          (member harm-interval2 intervals)))
                   (case mode
                     (:open-and-hidden
                      (if matching-interval2?
                          (/= (signum (- pitch1a pitch2a))
                              (signum (- pitch1b pitch2b)))
                          t))
                     (:open
                      (if matching-interval2?
                          (/= (mod (abs (- pitch1b pitch1a)) 1200)
                              harm-interval2)
                          t))))
                 t))))
    (loop for tail on vs append
          (loop for voice2 in (rest tail)
                append (ce::R-pitch-pitch rule (list (first tail) voice2) '(0)
                                           :all gracenotes? :pitch
                                           rule-type weight)))))

;;; ---------------------------------------------------------------------------
;;; UTILITIES
;;; ---------------------------------------------------------------------------

(om::defmethod! om-cr::scale->pitchdomain
    (scale-pitches &key (min 6000) (max 7200))
  :icon 1
  :initvals '(nil 6000 7200)
  :indoc '("scale pitches or PCs" "minimum pitch (midicents)" "maximum pitch (midicents)")
  :doc "Build a Cluster-Engine pitch domain from scale pitch classes using OM midicents."
  (let ((pcs (remove-duplicates (mapcar #'%pc-spec scale-pitches))))
    (loop for pitch from min to max by 100
          when (member (%mc-pc pitch) pcs)
          collect (list pitch))))

(om::defmethod! om-cr::file-in-this-directory ((filename string))
  :icon 1 :initvals '("") :indoc '("filename")
  :doc "Return a pathname in the library source directory."
  (rule::file-in-this-directory filename))

(om::defmethod! om-cr::read-lisp-file (path)
  :icon 1 :initvals '(nil) :indoc '("pathname")
  :doc "Read and return the first Lisp form in a file."
  (rule::read-lisp-file path))

(om::defmethod! om-cr::pprint-to-file (path expr)
  :icon 1 :initvals '(nil nil) :indoc '("pathname" "expression")
  :doc "Pretty-print a Lisp expression to a file."
  (rule::pprint-to-file path expr))

(om::defmethod! om-cr::map-pairwise (fn xs)
  :icon 1 :initvals '(nil nil) :indoc '("binary function" "list")
  :doc "Apply FN to every unordered pair of elements in XS."
  (rule::map-pairwise fn xs))

(om::defmethod! om-cr::mappend (func &rest inlists)
  :icon 1 :initvals '(nil nil) :indoc '("function" "lists")
  :doc "Map FUNC over one or more lists and append the results."
  (apply #'rule::mappend func inlists))
