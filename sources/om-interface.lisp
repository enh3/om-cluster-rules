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
  :doc "Heuristic rule. The pitches or rhythmic values of the resulting music follow the given profile, taking the timing of the profile into account. For example, with profile-duration set to 2 the profile (a fenv) ranges from 0 to 2 so that the fenv describes a profile over 2 whole notes.

Args:
- profile (a list of numbers, a fenv or -- when using Opusmodus -- an OMN sequence): Specifies the profile that should be followed. A list of numbers is translated into a fenv with equidistant points. If an OMN expression contains chords, then only the first chord note is extracted. If a list of profiles is specified, then these are used to constrain multiple given different voices. In this case, the number of profiles, and the number of specified voices to constrain must match.

- voices (int or list of ints): The voice(s) to which the constraint is applied. If multiple profiles are given, then a different voice for each profile should be specified.

- profile-duration (number): The overall duration of the profile. Even if the profile is specified as OMN expression with rhythmic values, by default the profile lasts for a whole tone (1/1) -- all rhythmic values are streched/shrinked accordingly.

- start / end (number or list of numbers): At which time point to start / end applying this rule. A start time greater zero has the effect that the profile is shifted in time to start at the specified time. An end time smaller than the duration of the profile has the effect that the part of the profile behind the set end is cut off. If end is NIL (the default) then the full profile duration is used.
start and end can both also be a list of start/end values to specify different values for different voices.

- mode: Select whether to constrain the rhythmic values (rhythm) or the pitches (pitch). If you want to constrain both, then simply use two instances of this rule with different mode settings.

- constrain: Select whether pitch/rhythm should follow the profile directly, or whether pitch/rhythm intervals should follow the intervals between profile, or pitch/rhythm directions should follow the directions of profile intervals.

- interpolate-score? (only relevant if profile is an OMN expression): Specifies whether score pitches or durations should be hold for their whole duration in the profile (sample-and-hold format), or whether between these values should be interpolated (zick-zack format).

- weight-offset (int): offset to the heuristic weight of this rule (the higher the offset, the more important this rule becomes compared with other heuristic rules).

Other arguments are inherited from hr-rhythm-pitch-one-voice.

NOTE: If this rule is used with pitch/rhythm motifs, then only the selection of the 1st motif note is controlled by the rule.

BUG:
A profile as OMN expression with leading rests not yet properly supported.

OM BPF/BPF-LIB inputs are converted internally to FENV."
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
  :doc "Heuristic rule. The pitches or rhythmic values of the resulting music follow the given profile.

Args:

- profile (a list of numbers, a fenv, or -- when using Opusmodus -- an OMN sequence, or a list of any of these): Specifies the profile to be followed. In case an OMN sequence contains chords, then only the first chord note is extracted. In case a fenv is given, then that fenv is sampled (n equidistant samples) and the y values are used. If multiple profiles are given, they are applied to the given voices in the same order.

- voices (int or list of ints): The voice(s) to which the constraint is applied.

- n (int): The first n notes are affected (if n is greater than the length of profile, then that length is taken). If 0, then n is disregarded and the full length of the profile is used. NOTE: if the profile is a fenv then make sure n is greater than 0.

- mode: Select whether to constrain either the rhythmic values (rhythm) or the pitches (pitch). If you want to constrain both, then simply use two instances of this rule with different mode settings.

- constrain: Select whether pitch/rhythm should follow the profile directly, or whether pitch/rhythm intervals should follow the intervals between profile, or pitch/rhythm directions should follow the directions of profile intervals.

- start (int): At which note position to start applying this rule (zero-based).

- weight-offset (int): offset to the heuristic weight of this rule (the higher the offset, the more important this rule becomes compared with other heuristic rules).

NOTE: If this rule is used with pitch/rhythm motifs, then only the selection of the 1st motif note is controlled by the rule (in future it would be nice to control the average pitch/rhythm of motifs, but that would require different rule applicators).

BUG: mode :rhythm not yet working.

OM BPF/BPF-LIB inputs are converted internally to FENV."
  (rule::follow-profile-hr (%profile->fenv profile)
                           :voices voices :n n :mode mode :constrain constrain
                           :start start :weight-offset weight-offset))

(om::defmethod! om-cr::follow-interval-profile
    (profile &key (voices 0) (n 0) (step-size 200) (start 0))
  :icon 1
  :initvals '(nil 0 0 200 0)
  :indoc '("profile (list or BPF)" "voices" "number of notes" "step size (midicents)" "start")
  :doc "Strict rule: The pitches of the resulting music follow the intervals of the given profile (numbers, a voice, or BPFs). If the profile contains a pitch repetition, then the corresponding interval in the solution must be a repetition. If the profile contains a step (up to `step-size'), then the corresponding interval in the solution must be a step in the same direction (up to `step-size'). If the profile contains a skip (larger than `step-size'), then the corresponding interval in the solution must be a skip in the same direction.

Args:

voices (int or list of ints): The voice(s) to which the constraint is applied.

n (int): The first n notes are affected (if n is greater than the length of profile, then that length is taken). If 0, then n is disregarded. NOTE: if a BPF is used then make sure n is greater than 0.

profile: Specifies the profile, which should be followed. This can be either a list of numbers (ints, floats or ratios), a voice object (or a score/part), a BPF object. In case a score or part object is given, then only the first voice is extracted and used. If voice objects contains chords, then only the first chord note is extracted. In case a BPF is given, then that BPF is sampled (n samples) and the y values are used.

Key args:

start (int): At which note position to start applying this rule (zero-based).

TODO: revise -- is this part of doc (copied from previous version of rule) still true?
NOTE: If this rule is used with pitch/rhythm motifs, then only the selection of the 1st motif note is controlled by the rule (in future it would be nice to control the average pitch/rhythm of motifs, but that would require different rule applicators).

This the OM port of the public PWGL box."
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
  :doc "Heuristic constraint: rhythmic values essentially follow a BPF. However, the BPF is slightly processed. Firstly, the BPF values are scaled into the interval [min-scaling, max-scaling]. Secondly, the BPF can be somewhat randomised (amount chosen with rnd-deviation, see below). Also, the BPF becomes somewhat 'curved' (using power 3) to address the distribution of rhythmic values (e.g., 1/16, 1/8, 1/4..) [the latter is a HACK].

Note that the rule follow-profile-hr is more flexible than the rule rhythm-profile-BPF-hr, but this rule is more easy to use for its purposes. Also, this rule allows for rests to occur at any position of a note (with the same duration).

Args:
  voices (int or list of ints): the voice(s) to which the constraint is applied.
  n (int): number of notes
  BPFs (a BPF or list of BPFs): the BPF to follow.
  min-scaling (positive number): min dur (e.g, 1/16).
  max-scaling (positive number): max dur.

Keyword args:
  rnd-deviation (float): amount by which the resulting value for the heuristic deviates from given BPF. 0 means no deviation, 0.5 means the value may deviate up to 50 percent (to either side).
  permutate (a function): arbitrary permutations of the BPF can be defined by a function expecting a list of numbers and returning a list of numbers of the same length. Such permutations are applied after all internal processing of the BPF.

NOTE: This rule can apply different BPFs to different voices with different settings. If a list of BPFs is given, then a different BPF is given to each of the voices listed. In that case, all other arguments (except n) can be either single values that are shared by all voices, or a list of different values for the different voices."
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
  :doc "Compose mapping/transformation functions from left to right into a single function that can be given to pitch-profile-hr or rhythm-profile-hr."
  (apply #'rule::compose-functions functions))

(om::defmethod! om-cr::mp-add-offset ((offset number))
  :icon 1 :initvals '(0) :indoc '("offset")
  :doc "Return a mapping function for pitch-profile-hr or rhythm-profile-hr that adds OFFSET to the original value."
  (rule::mp-add-offset offset))

(om::defmethod! om-cr::mp-multiply ((factor number))
  :icon 1 :initvals '(1) :indoc '("factor")
  :doc "Return a mapping function for pitch-profile-hr or rhythm-profile-hr that multiplies the original value by FACTOR."
  (rule::mp-multiply factor))

(om::defmethod! om-cr::mp-add-random-offset ((max-random-offset number))
  :icon 1 :initvals '(0) :indoc '("maximum random offset")
  :doc "Return a mapping function for pitch-profile-hr or rhythm-profile-hr that adds a random offset in +/- MAX-RANDOM-OFFSET, the maximum random deviation above or below the original value."
  (let ((a (abs max-random-offset)))
    (lambda (x) (+ x (%random-between (- a) a)))))

(om::defmethod! om-cr::trfm-scale ((minimum number) (maximum number))
  :icon 1 :initvals '(0 1) :indoc '("minimum" "maximum")
  :doc "Return a transformation function for pitch-profile-hr or rhythm-profile-hr that scales the original values between MINIMUM..MAXIMUM."
  (lambda (xs) (%scale-list xs minimum maximum)))

(om::defmethod! om-cr::trfm-add-bpf ((bpf om::bpf))
  :icon 1 :initvals '(nil) :indoc '("BPF")
  :doc "Return a transformation function for pitch-profile-hr or rhythm-profile-hr that adds the sampled BPF to each original value."
  (lambda (xs) (mapcar #'+ xs (%sample-profile bpf (length xs)))))

(om::defmethod! om-cr::trfm-multiply-bpf ((bpf om::bpf))
  :icon 1 :initvals '(nil) :indoc '("BPF")
  :doc "Return a transformation function for pitch-profile-hr or rhythm-profile-hr that multiplies each original value by the sampled BPF."
  (lambda (xs) (mapcar #'* xs (%sample-profile bpf (length xs)))))

(om::defmethod! om-cr::trfm-reverse ()
  :icon 1 :initvals nil
  :doc "Return a transformation function for pitch-profile-hr or rhythm-profile-hr that reverses the original value sequence."
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
  :doc "For any two consecutive beats/bars, at least one note must start on the beat. All arguments are inherited from r-meter-note."
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
  :doc "Prohibits syncopation with respect to the selected metric level. All arguments are inherited from r-meter-note."
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
  :doc "Restricts syncopations (of level metric-structure) to notes accented according to accent-rule.

Args:
  voices (int or list of ints): The numbers of voice(s) to constrain.

  accent-rule (menu item or function): A function returning true if an accent is expressed and nil otherwise. The function expects one or more arguments, all in the form (dur offs), where dur is the duration of a note and offs is the offset to the following accent (i.e. the duration until the following accent). Example: '(1/4 -1/8). A note is 'on' the accent if its offset = 0.
Some accent rules are predefined and can be simply selected in the menu of the argument. Other predefined accent rules expect additional arguments controlling their effect. These are available under the Cluster Rules sub menu rhythm - accent rules.

Other arguments are inherited from r-rhythm-rhythm.

TMP: doc
BUG: Still not working. See example ShiftedMetricAccents (for CIM paper).
I included debugging format instruction, but seeminlgy this not printed for every note. Double-check for which notes this skipped.
Anyway, I may be close with this one...

TODO: Include in rhythm menu, once finished."
  (rule::with-cr-error-log ("om-cr::no-syncopation-unless-accented")
    (rule::no-syncopation-unless-accented :voices voices
                                          :metric-structure metric-structure
                                          :accent-rule accent-rule
                                          :rule-type rule-type :weight weight)))

(om::defmethod! om-cr::only-simple-syncopations
    (&key (voices 0) (gracenote-mode :normal)
          (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 :normal :true/false 1)
  :indoc '("voices" "grace notes" "rule type" "weight")
  :menuins '((1 (("normal" :normal) ("exclude gracenotes" :exclude-gracenotes)))
             (2 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Restricts syncopations over beats to certain relatively simple cases. For example, the only possible syncopation allowed for a note value 1/4 is 1/8 before a beat. All arguments are inherited from r-note-meter."
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
  :doc "Restricts the rhythmic position of notes to relatively simple cases. For example, triplet notes can only be part of a triplet. All arguments are inherited from r-note-meter."
  (rule::only-simple-tuplet-offs :voices voices :gracenote-mode gracenote-mode
                                 :rule-type rule-type :weight weight))

(om::defmethod! om-cr::start-with-rest
    (&key (rest-dur 0) (voices 0) (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(0 0 :true/false 1)
  :indoc '("rest duration (or domain)" "voices" "rule type" "weight")
  :menuins '((2 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Start the given voice(s) with a rest of the given duration (either an int or a list of ints indicating a domain) If rest-dur is NIL then this means a rest of any duration is acceptable.

Hint: make sure you included rests in your rhythm domain (as negative integers).

Other optional arguments are inherited from r-index-rhythms-one-voice."
  (rule::with-cr-error-log ("om-cr::start-with-rest")
    (rule::start-with-rest :rest-dur rest-dur :voices voices
                           :rule-type rule-type :weight weight)))

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
  :doc "Motifs must start metric-offset away from set beat. Motifs starting with rests are not constrained.

Args:
  metric-offset (ratio): How far should motifs be shifted with respect to the metric-structure? For example, if metric-offset is -1/8, then motifs will be shifted to start an eighths note before the beat (or bar).
  grid (ratio): Motifs could be longer than a beat (or bar) so that this rule would be checked more than once. If a motif is longer than the set grid, it will only be checked at its beginning whether its metric offset is as set.

Optional arg:
  min-motif-length (int): motifs with a length below this setting are uneffected.

Other arguments are inherited from r-meter-note."
  (rule::with-cr-error-log ("om-cr::metric-offset-of-motif")
    (rule::metric-offset-of-motif :metric-offset metric-offset :voices voices
                                  :metric-structure metric-structure :grid grid
                                  :min-motif-length min-motif-length
                                  :rule-type rule-type :weight weight)))

(om::defmethod! om-cr::phrase-length
    (phrase-length &key (voices 0) (relation :min) (n 32)
                   (rule-type :true/false) (weight 1))
  :icon 1
  :initvals '(4 0 :min 32 :true/false 1)
  :indoc '("phrase length (number/list/BPF/FENV)" "voices" "relation"
           "number of profile samples" "rule type" "weight")
  :menuins '((2 (("minimum" :min) ("maximum" :max)))
             (4 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "This rule controls the number of notes and grace notes between rests (the length of phrases).

Args:
  phrase-length (int): The set number of notes between rests. Consecutive rests (effectively longer rests) can occur freely.
  relation: Whether the set phrase length is the required minimum or maximum. (If you want to constrain both the upper and lower boundary then simply use two of these rules.)

BUG: Strangely, at least one motif of the rhythm domain must have at least length 2. Not yet sure why..

Other arguments are inherited from r-rhythms-one-voice."
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
  :doc "This rule restricts the maximum difference between simultaneous note durations. Together with the rule r-rhythm-hierarchy (from library cluster engine) this rule allows to enforce a homophonic texture and also almost homophonic textures.

Note that grace notes are ignored by this rule.

Args:
  voices (a list of ints): The list of voices affected by this constraint. The first given voice is used as a reference: notes of all other voices are compared to this voice. NOTE: if the reference voice is not the voice with the lowest number, then the search is slowed down.
  max-factor (a ratio): If max-factor is 1, then the simultaneous durations always have exactly the same duration (however, neither the size of the duration nor whether they start together is constrained). If max-factor is larger (or smaller) than one then this factor defines the largest possible quotient between simultaneous durations. For example, if max-factor is 2 then any note can be at most the double and at least halve of the simultaneous note (note that the rule behaves the same whether max-factor is 1/2 or 2).
  rest-mode: Whether or not to also constrain rests or not. If rests are constrained, then all simultaneous notes must be notes and simultaneous rests must be rests.

Other args are inherited from r-rhythm-rhythm.

BUG: Arg factor seemingly not fully working as documented yet if factor > 1."
  (rule::with-cr-error-log ("om-cr::similar-sim-durations")
    (rule::similar-sim-durations :voices voices :max-factor max-factor
                                 :rest-mode rest-mode :rule-type rule-type
                                 :weight weight)))

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
  :doc "Restricts where metric accents occur depending on the underlying meter. If an accent occurs, then it is on the position defined.

Args:
  metric-structure: Position where accents are controlled (on any beat or the first beat of a measure).

  accent-rule (menu item or function): A function returning true if an accent is expressed and nil otherwise. The function expects one or more arguments, all by default (if format is :d_offs) in the form (dur offs), where dur is the duration of a note and offs is the offset to the following accent (i.e. the duration until the following accent). Example: '(1/4 -1/8). A note is 'on' the accent if its offset = 0.
Some accent rules are predefined and can be simply selected in the menu of the argument. Other predefined accent rules expect additional arguments controlling their effect. These are available under the Cluster Rules sub menu rhythm - accent rules.

  When accent-rule expects multiple arguments (data for multiple consecutive notes), on which note the metric accent is forced to occur depends on the number of arguments expected by accent-rule.
    - 1 argument: that note.
    - 2 arguments: the second note.
    - 3 arguments: the second note.
    - 4 arguments: the fourth note.
    More arguments are currently not supported.

  Some accent rules are predefined and can be simply selected in the menu of the argument.
    :longer-than-predecessor: Accented notes are longer than the preceding note and at least as long as the succeeding note. BUG: not constrained for first and last 2 notes! (fixing that needs more flexible rule applicators).
    :longer-than-neighbours: Accented notes are longer than the preceding and the succeeding note. BUG: not constrained for first and last 2 notes!

  Other predefined accent rules expect additional arguments controlling their effect. These are available under the Cluster Rules sub menu rhythm - accent rules.

  strictness: Controls how events are constrained. There are three different cases.
    :note: if an event meets the accent-rule, then it must be on a specified metric position (see metric-structure). However, there can be such metric positions without notes meeting the accent-rule.
    :position: if an event is on a specified metric position (see metric-structure) then it must meet the accent-rule. However, there can be accentuated notes at other metric positions. Also, if a note continues sounding at the specified position that started earlier (syncopation) then no accent at that position can be enforced (because only notes are checked, not metric positions).
    :note-n-position: if an event meets the accent-rule, then it must be on a specified metric position -- and vice versa.

Other arguments are inherited from r-note-meter."
  (rule::with-cr-error-log ("om-cr::metric-accents")
    (rule::metric-accents :voices voices :metric-structure metric-structure
                          :accent-rule accent-rule :strictness strictness
                          :format format :gracenote-mode gracenote-mode
                          :rule-type rule-type :weight weight)))

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
  :doc "Restricts where metric accents occur depending on the note onsets defined in an 'accents voice'. If an accent occurs, then it is on the position defined.

Args:
  voices (int or list of ints): The numbers of voice(s) to constrain.

  accent-rule (menu item or function): A function returning true if an accent is expressed and nil otherwise. The function expects one or more arguments, all in the form (dur offs), where dur is the duration of a note and offs is the offset to the following accent (i.e. the duration until the following accent). Example: '(1/4 -1/8). A note is 'on' the accent if its offset = 0.
Some accent rules are predefined and can be simply selected in the menu of the argument. Other predefined accent rules expect additional arguments controlling their effect. These are available under the Cluster Rules sub menu rhythm - accent rules.

  strictness: Controls how events are constrained. There are three different cases.
    :note: if an event meets the accent-rule, then it must be on accented position (there is a simultaneous note onset in the accent-voice). However, there can be such accented positions without notes meeting the accent-rule.
    :position: if an event is on an accented position then it must meet the accent-rule. However, there can be accentuated notes at other positions. Also, if a note continues sounding at the accented position that started earlier then no accent at that position can be enforced (because only notes in voices are checked, not in accents-voice).
    :note-n-position: if an event meets the accent-rule, then it must be on a specified accented position -- and vice versa.

Optional args:

  accents-voice: the number of the voice that defines accents. Each note onset in accents-voice is taken as an accent for the given voices.

Other arguments are inherited from r-rhythm-rhythm."
  (rule::with-cr-error-log ("om-cr::accents-in-other-voice")
    (rule::accents-in-other-voice :voices voices :accents-voice accents-voice
                                  :accent-rule accent-rule :strictness strictness
                                  :rule-type rule-type :weight weight)))

(om::defmethod! om-cr::mk-accent-has-at-least-duration-ar
    (&key (min-duration 1/4))
  :icon 1 :initvals '(1/4) :indoc '("minimum duration")
  :doc "Returns an accent-rule function for metric-accents or accents-in-other-voice requiring at least MIN-DURATION."
  (rule::mk-accent-has-at-least-duration-ar :min-duration min-duration))

(om::defmethod! om-cr::mk-accent->-prep-or->=-dur-ar
    (&key (min-duration 1/4))
  :icon 1 :initvals '(1/4) :indoc '("minimum duration")
  :doc "Returns an accent rule for metric-accents or accents-in-other-voice. Accented notes are EITHER longer than the preceding note and at least as long as the succeeding note, OR at least min-duration long."
  (rule::mk-accent->-prep-OR->=-dur-ar :min-duration min-duration))

(om::defmethod! om-cr::mk-accent->-prep-and->=-dur-ar
    (&key (duration-threshold 1/4))
  :icon 1 :initvals '(1/4) :indoc '("duration threshold")
  :doc "Returns an accent rule for metric-accents or accents-in-other-voice. Accented notes are longer than the preceding note. Additionally, if the succeeding note is of the same length or longer, then they are at least duration-threshold long to count as accented."
  (rule::mk-accent->-prep-AND->=-dur-ar :duration-threshold duration-threshold))

(om::defmethod! om-cr::thomassen-accents ((midi-pitches list))
  :icon 1 :initvals '(nil) :indoc '("MIDI pitches")
  :doc "Expects a list of MIDI note numbers (ints) representing a melodic sequence, and returns a list of floats representing the associated melodic accent value of each pitch as defined by the Thomassen model (Thomassen, 1982).

NOTE: no accent values are available for the first two and the last pitch, therefore nil is return for those pitches.

A list of all potential accent strength values is shown below, obtained by systematically combining all values of the model (some of these values may not be possible in reality). High accent values (0.415) are mapped to accents expressed by local max prepared by two upwards steps. Slightly lower accent values (0.355) occur for accents prepared by two downward steps, followed by an upward step.

Accent value 0.335 seems to occurs for mere local max and min (or only local max?).

(0.0 0.028900001 0.0493 0.056100003 0.08409999 0.085 0.0957 0.10890001 0.113900006 0.120699994 0.1411 0.145 0.165 0.17 0.1943 0.20589998 0.22110002 0.2343 0.24069999 0.25 0.2739 0.29 0.33 0.335 0.355 0.415 0.4489 0.4757 0.5 0.50409997 0.5561 0.5893 0.67 0.6889 0.71 0.83 1.0)

* References:

Thomassen, J. M. (1982) Melodic accent: Experiments and a tentative model. The Journal of the Acoustical Society of America. 71 (6), 1596–1605."
  (rule::thomassen-accents midi-pitches))

(om::defmethod! om-cr::thomassen-accents-ar
    (&key (thomassen-accent-strength 0.4))
  :icon 1 :initvals '(0.4) :indoc '("accent strength threshold")
  :doc "Returns an accent rule for metric-accents or accents-in-other-voice. Accented notes are melodic accents according to the Thomassen model (Thomassen, 1982). See the documentation of thomassen-accents for more details on this model and the reference.

NOTE: This is an expensive constraint performance-wise (i.e. the search can take very long): the constraint defines a relation between four consecutive notes, the metric positions (offset value) of the but-last note and the melodic intervals of these four notes. Best use only for small and/or monophonic results.

NOTE: This constraint requires the format :d_offs_m_n for the functions metric-accents or accents-in-other-voice."
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
  :doc "Limit the minimum/maximum melodic interval for the given voice. In OM, interval values are expressed in midicents. BPF inputs are converted to FENV internally.

Args:
voices (int or list of ints): the number of the voice(s) to constrain.

key-args:
min-interval (number, fenv or NIL): minimum interval in midicents. Ignored if NIL. Implicitly disallows repetition if >= 1. If a fenv, then the fenv specifies how the min interval changes over n notes (i.e., fenv specifies n-1 intervals).
max-interval (number, fenv or NIL): maximum interval in midicents. Ignored if NIL. If a fenv, then the fenv specifies how the max interval changes over n notes.
n (int): The first n notes are affected. If 0, then n is disregarded. NOTE: if any fenv is set then make sure n is greater than 0.

Args rule-type and weight inherited from r-pitches-one-voice."
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
  :doc "Restricts the pitches to the pitches or PCs specified. Absolute pitches are OM midicents; pitch classes are integers 0..11.

Args:
  pitches (list of ints): Specified pitches.
  pcs?: use absolute pitches or pitch classes?
  mode: Controls whether to only use the given intervals (:only-given), or whether to only use intervals that are not given (:exclude-given).

Other arguments are inherited from r-pitches-one-voice."
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
  :doc "Restricts the melodic intervals to those intervals specified. In OM, interval values are expressed in midicents.

Args:
  intervals (list of ints): Specified intervals.
  absolute?: Controls whether the direction of the intervals is taken into account. The direction can be specified with a sign (positive for upwards, negative for downwards). If absolute? is set to :absolute, then the specified intervals can be used for any direction. By contrast, :up/down takes the sign of the given intervals into account.
  mode: Controls whether to only use the given intervals (:only-given), or whether to only use intervals that are not given (:exclude-given).

Other arguments are inherited from r-pitches-one-voice."
  (rule::set-intervals :intervals intervals :absolute? absolute? :mode mode
                       :voices voices :rule-type rule-type :weight weight))

(om::defmethod! om-cr::prefer-interval-hr
    (interval &key (voices 0) (n 0) (weight-factor 1))
  :icon 1
  :initvals '(100 0 0 1)
  :indoc '("preferred interval (midicents or BPF)" "voices" "number of notes" "weight factor")
  :doc "Heuristic rule that constrains the preferred melodic interval size. In OM, interval values are expressed in midicents. By default, small steps are preferred (interval is 0). The more an interval deviates from the set interval the less likely it is to be chosen.

Args:

voices (int or list of ints): The voice(s) to which the constraint is applied.

interval (number, BPF or FENV): The preferred interval size. A number sets a constant size, which a BPF sets a size that changes over time. NOTE: if a BPF is used then make sure arg n (see below) is greater than 0.

n (int): The first n notes are affected. If 0, then n is disregarded.

weight-factor: factor for the heuristic weight."
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
  :doc "The sum of melodic intervals between the pitches of n notes is smaller / greater than the given interval. OM interval values are midicents. If there are any rests among the last n notes then this rule is ignored.

Args:
  n (int): number of notes involved.
  interval (int or BPF): the max/min interval. If a BPF, then it defines how the interval changes across the voice(s). However, in that case the arg number-of-notes must be given, otherwise the rule throws an error.
  condition: the relation that should hold between the sum of intervals and the given interval: whether the sum should not exceed (:max) or be exactly (:equal) or be at least (:min) the given interval.

Optional args:
  sublists: whether or not to also constrain the intervals between the last n-1, n-2 ... notes in the same way. This argument is only effective if condition is set to :max, otherwise it setting is always :ignore.
  number-of-notes: the number of variables set in clusterengine. This argument is required if interval is a BPF.

Other arguments are inherited from r-pitches-one-voice."
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
  :doc "Disallows any direct pitch or chord repetition.

Args:
voices: the number of the voice(s) to constrain.

Optional arguments are inherited from r-pitches-one-voice."
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
  :doc "Disallows repetitions within a window of a given number of melodic notes or chords. Pitch-class mode is adapted to OM midicents.

Args:
voices: the number of the voice(s) to constrain.
window: the number of notes among which no repetition should happen (if this larger than the currently available number, then simply the available notes are taken).
mode: whether to disallow repeated pitches (:pitches) or pitch classes (:pcs).

Optional arguments are inherited from r-pitches-one-voice."
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
  :doc "Pitch intervals and durations are linearly related. The default relation factor is scaled for OM midicents.

Args:
voices (int or list of ints): The voice(s) to which the rule is applied.

rel-factor (relation factor): the size of the melodic interval is approximately the duration times rel-factor.

acc-factor (accuracy factor): factor how much the interval can deviate from that relation above and below.

Examples: If rel-factor is 1 and acc-factor is also one, then the duration of a note would need to be the same as the interval starting at it (e.g., duration = 2 and interval is 2). If rel-factor is 32 and acc-factor is 2 (the defaults) then the interval can be any value between duration*32/2 and duration*32*2.

Optional arguments are inherited from r-rhythm-pitch-one-voice."
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
  :doc "This rule controls how many consecutive intervals can be ascending or descending. At most n notes can be connected by intervals of the same direction.

Args:
  n (int): How many consecutive notes are taken into account?
  direction: Which interval direction is taken into account? For example, :ascending means that the rule only looks at consecutive ascending intervals. Without the ending -strict, note repetitions are also counted as the same direction.

In case of intermitting rests the rule is not applied.

Other arguments are inherited from r-pitches-one-voice."
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
  :doc "Resolve any skip larger than skip-size by an interval in the opposite direction. OM interval values are midicents.

Args:
  skip-size: The minimum interval size (in midicents) that triggers this rule.
  resolution-size: The maximum interval size that is allowed as a resolution.
  repetition?: Whether or not tone repetitions are allowed as resolution.

Other arguments are inherited from r-pitches-one-voice."
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
  :doc "Tones (PC) in the given voice must be a member of the underlying scale (its PCs). The scale is represented as a simultaneous chord in another voice (voice 0 by default). I is either given directly to the clusterengine's pitch domain of that scale voice, or using read-harmony-file, or controlled with other constraints on that voice.

Args:
voices (int or list of ints): the voice(s) to which this constraint is applied.

Optional args:
scale-voice (int, default 0): the voice representing the underlying scale.

Other arguments are inherited from r-pitch-pitch."
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
  :doc "Tones (PC) in the given voice must be a member of the underlying chord (its PCs). The chord is represented as a simultaneous chord in another voice (voice 1 by default). I is either given directly to the clusterengine's pitch domain of that scale voice, or using read-harmony-file, or controlled with other constraints on that voice.

Args:
voices (int or list of ints): the voice(s) to which this constraint is applied.

Optional args:
chord-voice (int, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch. For example, it is possible to control whether this constraint should be applied to all notes, or only specific notes (input-mode). By default, it is applied to notes starting on a beat."
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
  :doc "Pitches in the given voice must be a member of the underlying spectrum (its absolute pitches). The spectrum is represented as a simultaneous chord in another voice (voice 1 by default).

Args:
voices (int or list of ints): the voice(s) to which this constraint is applied.

Optional args:
spectrum-voice (int, default 1): the voice representing the underlying spectra (quasi underlying harmony).

Other arguments are inherited from r-pitch-pitch. For example, it is possible to control whether this constraint should be applied to all notes, or only specific notes (input-mode). By default, it is applied to notes starting on a beat.

This rule is very similar to only-chord-PCs, but instead of pitch classes absolute pitches are constrained to the pitches of the given spectrum (quasi chord)."
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
  :doc "Every note in the given voice(s) with a duration exceeding max-nonharmonic-dur (on any metric position) must be a harmonic tone (pitch-class comparison is adapted to OM midicents): the PC of such notes must be a member of the underlying chord (its PCs). The chord is represented as a simultaneous chord in another voice (voice 1 by default). I is either given directly to the clusterengine's pitch domain of that scale voice, or using read-harmony-file, or controlled with other constraints on that voice.

Args:
voices (int or list of ints): the voice(s) to which this constraint is applied.
max-nonharmonic-dur (int): the maximum duration for which non-harmonic pitches are permitted.

Optional args:
chord-voice (int, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch. For example, it is possible to control whether this constraint should be applied to all notes, or only specific notes (input-mode). By default, it is applied to notes starting on a beat."
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
  :doc "Tones (PC) in the given voice(s) after a rest must be a member of the underlying chord PCs.

Args:
voices (int or list of ints): the voice(s) to which this constraint is applied.

Optional args:
chord-voice (int, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch."
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
  :doc "HACK: The very first tone (PC) in given voice(s) number must be a member of the first of the given chords (the list of list of chords from read-harmony-file).

NOTE: an index variant for a pitch-pitch constraint (which could access the sim chord PCs) is not available, therefore this workaround.

Other arguments are inherited from r-index-pitches-one-voice."
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
  :doc "Every tone (PC) that is not a chord tone (member of sim chord PCs, in voice 1 by default) is reached/left by a step of the given step size.

Args:
step-size (int): maximum interval considered a step.
voices (int or list of ints): the voice(s) to which this constraint is applied.

Optional args:
chord-voice (int, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch."
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
  :doc "Every tone (PC) that is not a chord tone (member of sim chord PCs, in voice 1 by default) is followed by a chord tone.

Args:
voices (int or list of ints): the voice(s) to which this constraint is applied.

Optional args:
chord-voice (int, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch."
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
  :doc "Sim PCs in all given voices are unequal to each other.

Arguments are inherited from r-pitch-pitch.

TODO: Revise this definition -- can the interplay with unequal-sim-PCs-aux be simplified?"
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
  :doc "Controls the number of simultaneous pitch classes. Useful, for example, to require that some underlying harmony is expressed.

Args:
  PC-number (int): the number of the simultaneous PCs. The meaning of this setting depends on the argument condition.
  condition: Whether the number of simultaneous pitch classes should be at least the given PC-number (:min), or exactly that number (:equal), or at most that number (:max).
  rests-mode: If set to :reduce-no, then the number of simultaneous pitch classes is subtracted from PC-number. For example, if there is only a single tone at a certain time and all other voices have rests, this rule can still be fulfilled. By contrast, if rests-mode is set to :ignore, then the remaining simultaneous pitch classes must still fullfil the condition expressed by the arguments PC-number and condition.
  voices: the list of voices to which the rule is applied.

Other arguments are inherited from r-pitch-pitch."
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
  :doc "Restricts the harmonic intervals between all combinations of the given voices to only (or not) those intervals specified. Absolute intervals are midicents; PC intervals are 0..11. For example, 'empty' perfect consonances in two-voice counterpoint can be excluded with this rule.

Args:
  intervals (list of ints): Specified intervals.
  pcs?: whether absolute intervals or PC intervals should be used.
  exclude/only: Controls whether to only use the given intervals (:only-given), or whether to only use intervals that are not given (:exclude-given).
  combinations: Controls whether to constrain only intervals between the bass and a higher voice (:over-bass), between pairs of consecutive voices such as soprano-alto, alto-tenor etc. (:consecutive-voices), or between all voice combinations (:all-combinations).

Other arguments are inherited from r-pitch-pitch."
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
  :doc "Limit the minimum/maximum harmonic interval of simultaneous notes between given voices. In OM, interval values are expressed in midicents.

Args:
  voices (list of ints): the voices to constrain.
  min-interval (number or NIL): minimum interval in midicents. Ignored if NIL.
  max-interval (number or NIL): maximum interval in midicents. Ignored if NIL.
  combinations: Controls whether to constrain only intervals between the voice with highest note number and other voices (:over-bass), between pairs of consecutive voices such as soprano-alto, alto-tenor etc. (:consecutive-voices), or between all voice combinations (:all-combinations).

Other arguments are inherited from r-pitch-pitch."
  (rule::min/max-harmonic-interval :voices voices :min-interval min-interval :max-interval max-interval
                                   :input-mode input-mode :combinations combinations
                                   :gracenotes? gracenotes? :timepoints timepoints
                                   :rule-type rule-type :weight weight))

(om::defmethod! om-cr::tintinnabuli-m-voice
    (&key (voices 0) (max-interval 200) (rule-type :true/false) (weight 1) (scale-voice 0))
  :icon 1 :initvals '(0 200 :true/false 1 0)
  :indoc '("voices" "maximum melodic interval (midicents)" "rule type" "weight" "scale voice")
  :menuins '((2 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Rules for a tintinnabuli M-voice, inspired by Arvo Pärt (slightly generalised, because this rule is applicable over any harmony). The voice consists only of scale tones that move stepwise (max interval is whole tone). This rule is adapted to OM midicents.

Args:
voices (int or list of ints): the number of the voice(s) to constrain.

Optional:
max-interval (default 200): maximum interval in midicents.
scale-voice (default 0): the voice representing the underlying scale.

Other arguments are inherited from r-pitches-one-voice and r-pitch-pitch."
  (ce::rules-to-cluster
   (om-cr::min/max-interval :voices voices :max-interval max-interval :rule-type rule-type :weight weight)
   (om-cr::only-scale-pcs :voices voices :input-mode :all :rule-type rule-type :weight weight :scale-voice scale-voice)))

(om::defmethod! om-cr::tintinnabuli-t-voice
    (&key (voices 0) (min-interval 0) (max-interval 1200)
          (rule-type :true/false) (weight 1) (chord-voice 1))
  :icon 1 :initvals '(0 0 1200 :true/false 1 1)
  :indoc '("voices" "minimum melodic interval (midicents)" "maximum melodic interval (midicents)" "rule type" "weight" "chord voice")
  :menuins '((3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Rules for a tintinnabuli T-voice, inspired by Arvo Pärt (slightly generalised, because this rule is applicable over any harmony). The voice consists only of chord tones, and the minimum/maximum interval size can be controlled. This rule is adapted to OM midicents.

Args:
voices (int or list of ints): the number of the voice(s) to constrain.

Key args:
min-interval (default 0): minimum interval in midicents.
max-interval (default 1200): maximum interval in midicents.
chord-voice (default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitches-one-voice and r-pitch-pitch."
  (ce::rules-to-cluster
   (om-cr::min/max-interval :voices voices :min-interval min-interval :max-interval max-interval :rule-type rule-type :weight weight)
   (om-cr::only-chord-pcs :voices voices :input-mode :all :rule-type rule-type :weight weight :chord-voice chord-voice)))

(om::defmethod! om-cr::set-chord-at-positions
    (positions chord &key (chord-voice 1) (rule-type :true/false) (weight 1))
  :icon 1 :initvals '(nil nil 1 :true/false 1)
  :indoc '("positions" "chord" "chord voice" "rule type" "weight")
  :menuins '((3 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Restricts the chords at the given positions (0-based) in the chord-voice to the given chord.

Other arguments are inherited from r-index-pitches-one-voice."
  (rule::set-chord-at-positions positions chord :chord-voice chord-voice :rule-type rule-type :weight weight))

(om::defmethod! om-cr::set-root-at-positions
    (positions root &key (pc? t) (chord-voice 1) (rule-type :true/false) (weight 1))
  :icon 1 :initvals '(nil 6000 t 1 :true/false 1)
  :indoc '("positions" "root (midicent or PC)" "compare pitch class" "chord voice" "rule type" "weight")
  :menuins '((2 (("pitch class" t) ("absolute pitch" nil)))
             (4 (("true/false" :true/false) ("heur-switch" :heur-switch))))
  :doc "Restricts the chord roots (the lowest chord pitches) at the given positions (0-based) in the chord-voice to the given root.

If pc? is set to T, then this constraint compares pitch classes instead of actual pitches.

Set roots at selected chord positions. Pitch-class comparison is adapted to OM midicents.

Other arguments are inherited from r-index-pitches-one-voice."
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
  :doc "Constrains the voice leading distance of consecutive chords in chord-voice to be at most max-distance.

The voice leading distance between two given chords, each a list of MIDI note numbers, is an integer measured in semitones. It is the minimal sum of intervals between chord1 and chord2. The voice-leading distance is directionless in the sense that regardless whether a voice moves up or down, always the smaller interval is taken into account. The lower the voice leading distance, the more 'smooth' is the harmonic progression (a chord repetition is quasi most smooth).

Currently, only 12-TET is supported.

Example: voice leading distance between C major and Ab major triads
(voice-leading-distance '(60 64 67) '(56 60 63))
=> 2
C->C=0 + E->Eb=1 + G->Ab=1, so the sum is 2

Note: Only the minimal intervals from all chord2 pitch classes to chord1 pitch classes are taken into account. There may be pitch classes in chord1 which are ignored as all pitch classes of chord2 may be closer to some other pitch classes of chord1.

Example: C-maj -> F#-maj = 4
(voice-leading-distance '(60 64 67) '(66 70 73))
=> 4
C->C#=1, C->A#=2, G->F#=1 -- the E of C-maj is ignored in the computation

If `n' is set, only the first `n' pitch classes of the chords are taken into account.

  Args:
  - chord-voice (int): the voice representing the underlying chord.
  - n (int): only the first `n' pitch classes of chords are taken into account, if this argument is set.

Limit voice-leading distance between successive chords; OM distance is expressed in midicents.

Other arguments are inherited from r-pitches-one-voice."
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
  :doc "Rule that constrains consecutive chords to an ascending progression. By default, this is an heuristic rule.

A progression from chord1 to chord2 is ascending (or strong) if chord1 and chord2 have common pitch classes, but the root of chord2 does not occur in the set of pitch classes of chord1.

Such a definition is less restrictive than Schoenberg's original guidelines (e.g., a root progression by a step upwards into a 7th chord also counts as an ascending progression here). Therefore, the rule supports the additional argument n: only the first n pitches of chord1 and chord2 are taken into account, if this argument is set.

Example:

By default, a root progression by a step upwards into a 7th chord also counts as an ascending progression.
;; (ascending-progression? '(60 64 67) '(62 66 69 72)) ; T

That is not the case, if only the first 3 distinct pitch classes are taken into account, which can be set with n. Note that the order of the pitches (and thus pitch classes) matters here -- the seventh is the 4th pitch in the chord. If you do not want certain chord pitches to be taken into account, you have to make sure that these pitches occur above the threshold n.
;; (ascending-progression? '(60 64 67) '(62 66 69 72) 3) ; nil

  Args:
  - chord-voice (int): the voice representing the underlying chord.
  - n (int): only the first n pitch classes of chords are taken into account, if this argument is set.Other arguments are inherited from r-pitches-one-voice.

  Music representation convention:
  - A chord/spectrum/scale is represented as a list of pitches or pitch classes.
  - Pitches are represented as MIDI note numbers, pitch classes as an integers between 0 and 11 (currently limited to 12-TET).
  - The root of a chord/spectrum/scale is its first pitch (class)."
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
  :doc "Rule that constrains a chord progression according to Schoenberg's recommendation. For any three successive chords/scales, if the first two chords form a descending progression, then the progression from the first to the third chord forms a strong progression (so the middle chord is quasi a 'passing chord'). Also, the last chord/scale pair forms always a strong progression.

  Args:
  - allow-interchange-progression (Boolean): If true, then mere interchange progressions (e.g., I V I), are permitted as well. In any case, no two descending progressions must follow each other.
  - allow-repetition (Boolean): If true, two consecutive chords can have the same root.
  - chord-voice (int): the voice representing the underlying chord.
  - n (int): only the first n pitch classes of chords are taken into account, if this argument is set.Other arguments are inherited from r-pitches-one-voice.

  Music representation convention:
  - A chord/spectrum/scale is represented as a list of pitches or pitch classes.
  - Pitches are represented as MIDI note numbers, pitch classes as an integers between 0 and 11 (currently limited to 12-TET).
  - The root of a chord/spectrum/scale is its first pitch (class)."
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
  :doc "[Convenience constraint] Constraints the chord root progression of consecutive chords, but different values of `progression' set different variants of Schoenbergs rule set. Supported values for `progression' are as follows.

   - :ascending - only ascending chord progressions are permitted.
   - (:resolve-descending-progression &rest args) - descending progressions are resolved (arguments to rule resolve-descending-progression can be given as further values in this list).
   - :harmonic-band - consecutive chords must share common pitch classes.
   - :common-pcs - consecutive chords must share common pitch classes.

  Args:
  - chord-voice (int): the voice representing the underlying chord.
  - n (int): only the first n pitch classes of chords are taken into account, if this argument is set.Other arguments are inherited from r-pitches-one-voice.

  Music representation convention:
  - A chord/spectrum/scale is represented as a list of pitches or pitch classes.
  - Pitches are represented as MIDI note numbers, pitch classes as an integers between 0 and 11 (currently limited to 12-TET).
  - The root of a chord/spectrum/scale is its first pitch (class)."
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
  :doc "Voices should not cross, i.e., the pitch of simultaneous note pairs in voices are always sorted in decreasing order.

Arguments are inherited from r-pitch-pitch."
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
  :doc "Parallels of given intervals are prohibited between all combinations of the given voices. OM interval values are midicents.

Args:
  mode: Specifies whether only open or also hidden intervals should be avoided.
  intervals (list of ints): Specifies the intervals (as midicents modulo octave) of which parallels should be avoided.

Other arguments are inherited from r-pitch-pitch."
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
  :doc "Expects a list of pitches representing a scale (either pitch classes or absolute pitches), and a minimum and maximum pitch. Returns a pitch domain for clusterengine that contains all pitches between the min and max in the scale. All pitch values are expressed as OM midicents."
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
  :doc "Expects a path name to a lisp file and returns the read (but not evaluated) content of the file. For example, if the file ; contains (1 2 3) then the list (1 2 3) is return (i.e., not a string, but a list, but 1 is not called as a function). Note that only the 1st Lisp form in path is returned."
  (rule::read-lisp-file path))

(om::defmethod! om-cr::pprint-to-file (path expr)
  :icon 1 :initvals '(nil nil) :indoc '("pathname" "expression")
  :doc "Pretty-print a Lisp expression to a file."
  (rule::pprint-to-file path expr))

(om::defmethod! om-cr::map-pairwise (fn xs)
  :icon 1 :initvals '(nil nil) :indoc '("binary function" "list")
  :doc "Apply FN to every unordered pair of elements in XS and collect the results, i.e. ((fn xs1 xs2) .. (fn xs1 xsN) (fn xs2 xs3) .. (fn xsN-1 xsN))."
  (rule::map-pairwise fn xs))

(om::defmethod! om-cr::mappend (func &rest inlists)
  :icon 1 :initvals '(nil nil) :indoc '("function" "lists")
  :doc "Map FUNC over one or more lists and append the results."
  (apply #'rule::mappend func inlists))
