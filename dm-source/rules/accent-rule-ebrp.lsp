;; ----- Bisesi/Parncutt performed accent rules --------------------
;; ----- According to Friberg & Bisesi 2014 -------------------

;;Apply a curve in the sound level and/or the duration in the surrounding of an accent.

;;1002 af/eb : preliminary version
;;101210/af/eb New definitions, fixing more curves, additive but only sound level
;;adapted to graphical interface as defined in accentdialog-win
;;101213/af added accent-dr, integrated them in the rule system with support for polyphony, 
;; multiple rules at the same time
;; a general quantity parameter
;;111111/af new version again wih individual rules for each accent
;;120313/af adjusted accent quantities
;;120314/af added width scaling
;;120417/af added dynamic-accent
;;220616/af cleaned the code keeping only the last version, made them additive as all other rules, saved the old one in a separate file


(in-package :dm)


;;;----  Main functions for applying an accent on a marked note ------

;; each rules expect an annotation of an accent mark (accent-c, accent-h, accent-m)
;; together with a salience number (from 1-5)

;; quant  the general quantity of the rule, default 1
;; curve  :linear :quadratic :cubic :exponential :cosine :gaussian :hand-gesture
;; amp    scaling parameter for sound level, default 1 (no scaling)
;; dur    scaling parameter for duration (IOI), default 1 (no scaling)
;; width  scaling parameter for width, default 1 (no scaling)

;; 220620 Note that a linear scaling of the salience is done resulting in a positive value of 1.875 even when the marked salience is zero
;; The reason is that a certain threshold is needed in order to perceive the performed accent
;; However, when almost all notes are marked with an accent it is not working properly
;; In order to get zero resulting salience then a salience value of -3 has to marked on the note

;new accent only for dynamic variation
;tempo variation possible if the :dur keyword is used with a value greater than 0
;tag in the score: accent-d
(defun dynamic-accent (quant &key (curve :linear) (amp 1) (dur 0) (width 1))
  (each-note-if
   (this 'accent-d)
   (then
    (let* ((sal (this 'accent-d))
           (sal2 (+ (* 0.625 sal) 1.875))
           (w1 (* width 250.0 sal2))
           (w2 (* width 250.0 sal2))
           (quant-sl (* quant amp 2 sal2))
           (quant-dr (* quant dur 0.05 sal2))
          )
      (accent-apply-sl *i* w1 w2 quant-sl curve curve)
      (accent-apply-dr *i* w1 w2 quant-dr curve curve)
      )))
  ;transfer dsl and ddr to sl and dr additive/multiplicative
  (each-note-if (this 'sl) (add-this 'sl (or (this 'dsl) 0)))
  (each-note-if (this 'ddr) (set-this  'dr (* (this 'dr) (1+ (this 'ddr)))))
  (rem-all 'dsl)
  (rem-all 'ddr)
  )

(defun melodic-contour-accent (quant &key (curve :linear) (amp 1) (dur 1) (width 1))
  (each-note-if
   (this 'accent-c)
   (then
    (let* ((sal (this 'accent-c))
           (sal2 (+ (* 0.625 sal) 1.875))
           (w1 (* width 250.0 sal2))
           (w2 (* width 250.0 sal2))
           (quant-sl (* quant amp 2 sal2))
           (quant-dr (* quant dur 0.05 sal2))
          )
      (accent-apply-sl *i* w1 w2 quant-sl curve curve)
      (accent-apply-dr *i* w1 w2 quant-dr curve curve)
      )))
  ;transfer dsl and ddr to sl and dr additive/multiplicative
  (each-note-if (this 'sl) (add-this 'sl (or (this 'dsl) 0)))
  (each-note-if (this 'ddr) (set-this  'dr (* (this 'dr) (1+ (this 'ddr)))))
  (rem-all 'dsl)
  (rem-all 'ddr)
  )

(defun harmonic-accent (quant &key (curve :linear) (amp 1) (dur 1) (width 1))
  (each-note-if
   (this 'accent-h)
   (then
    (let* ((sal (this 'accent-h))
           (sal2 (+ (* 0.625 sal) 1.875))
           (w1 (* width 250.0 sal2))
           (w2 (* width 250.0 sal2))
           (quant-sl (* quant amp 2 sal2))
           (quant-dr (* quant dur 0.05 sal2))
          )
      (accent-apply-sl *i* w1 w2 quant-sl curve curve)
      (accent-apply-dr *i* w1 w2 quant-dr curve curve)
      )))
  ;transfer dsl and ddr to sl and dr additive/multiplicative
  (each-note-if (this 'sl) (add-this 'sl (or (this 'dsl) 0)))
  (each-note-if (this 'ddr) (set-this  'dr (* (this 'dr) (1+ (this 'ddr)))))
  (rem-all 'dsl)
  (rem-all 'ddr)
  )

(defun metrical-accent (quant &key (curve :linear) (amp 1) (dur 1) (width 1) (marker 'accent-m))
  (each-note-if
    (this marker)
    (then
      ;(print-ll "i = " *i*)
      (let* ((sal (this marker))
             (sal2 (+ (* 0.625 sal) 1.875))
             (w1 (* width 250.0 sal2))
             (w2 (* width 250.0 sal2))
             (quant-sl (* quant amp 2 sal2))
             (quant-dr (* quant dur 0.05 sal2))
             )
        (accent-apply-sl *i* w1 w2 quant-sl curve curve)
        (accent-apply-dr *i* w1 w2 quant-dr curve curve)
        )))
  ;transfer dsl and ddr to sl and dr additive/multiplicative
  (each-note-if (this 'sl) (add-this 'sl (or (this 'dsl) 0)))
  (each-note-if (this 'ddr) (set-this  'dr (* (this 'dr) (1+ (this 'ddr)))))
  (rem-all 'dsl)
  (rem-all 'ddr)
  )

;; ------- Apply the accents on the notes --------------

;; first make 'dsl and then add that to 'sl
;; with added floats marking time in ms
;; fractional time not yet implemented

;; inote     the note index for the accent
;; ext-left  left start, if float - time before inote in ms, if integer - number of notes before
;; ext-right same for end
;; peak      the peak in sl
;; curve-left, curve-right  the envelope function

;; with max instead of add so that envelopes don't add up - skipped
;; not really compatible with the traditional rule application but works well with the new rule interaction stuff
;; will probably not work with negative values as well
;; 120327/af
;; 130114/af included dsl (new score dynamics)
;; 220615 fixed problem with ndr in sync track, updated istart computation, integer in ext-left still used?
;; 220616 skipped the max value and changed back to normal additive function -- did not work for the interaction with itself for larger widths

#|
(defun accent-apply-sl (inote ext-left ext-right peak curve-left curve-right)
  ;(print-ll "accent-apply-sl input args: inote " inote " ext-left " ext-left " ext-right " ext-right " peak " peak " curve-left " curve-left " curve-right " curve-right)
  (let ((istart (if (float ext-left)
                    (i?ndr-before-index inote ext-left)
                  (max (- inote ext-left) 0) ))
        (iend (if (float ext-right)
                  (i?ndr-after-index inote ext-right)
                (min (+ inote ext-left) (i?last)) ))
        fun-left fun-right power-left power-right )
    ;translate from keywords to function names and power
    ;(print-ll "istart " istart " inote " inote " iend " iend)
    (multiple-value-setq (fun-left power-left) (accents-translate-curv-name-left curve-left))
    (multiple-value-setq  (fun-right power-right) (accents-translate-curv-name-right curve-right))
    (iset-ramp-x2-decimal-last istart inote 0.0 0.0 0.0 peak 'dsl power-left fun-left fun-left)
    (iset-ramp-x2-decimal-last inote iend 0.0 0.0 peak 0.0 'dsl power-right fun-right fun-right)
    ;transfer dsl to sl
    (loop for i from istart to iend do
          (if (iget i 'sl) 
              ;(iset i  'sl (max (iget i 'sl) (+ (if (iget i 'nsl) (iget i 'nsl) 0) (iget i 'dsl)))) ;why nsl here? It is not in sync track, changed so that if nsl is absent it uses 0
              (iadd i 'sl (iget i 'dsl)) )
          (rem-var (nth i *v*) 'dsl) )
    ))
|#

; 220616 added a new temp var dsl-temp that uses max, thus the interaction with itself is using the max value but with other rules additive as normal
#|
(defun accent-apply-sl (inote ext-left ext-right peak curve-left curve-right)
  ;(print-ll "accent-apply-sl input args: inote " inote " ext-left " ext-left " ext-right " ext-right " peak " peak " curve-left " curve-left " curve-right " curve-right)
  (let ((istart (if (float ext-left)
                    (i?ndr-before-index inote ext-left)
                  (max (- inote ext-left) 0) ))
        (iend (if (float ext-right)
                  (i?ndr-after-index inote ext-right)
                (min (+ inote ext-left) (i?last)) ))
        fun-left fun-right power-left power-right )
    ;translate from keywords to function names and power
    (print-ll "istart " istart " inote " inote " iend " iend)
    (multiple-value-setq (fun-left power-left) (accents-translate-curv-name-left curve-left))
    (multiple-value-setq  (fun-right power-right) (accents-translate-curv-name-right curve-right))
    (print-ll "istart " istart " inote " inote " iend " iend)
    (iset-ramp-x2-decimal-last istart inote 0.0 0.0 0.0 peak 'dsl-temp power-left fun-left fun-left)
    (print-ll "istart " istart " inote " inote " iend " iend)
    (iset-ramp-x2-decimal-last inote iend 0.0 0.0 peak 0.0 'dsl-temp power-right fun-right fun-right)
    (print-ll "istart " istart " inote " inote " iend " iend)
    (print-ll "dsl " (this 'dsl-temp))
    ;transfer dsl-temp to dsl using max
    (loop for i from istart to iend do
          (if (iget i 'sl) (iset i  'dsl (max (iget i 'dsl-temp) (or (iget i 'dsl) 0))))
          (rem-var (nth i *v*) 'dsl-temp)
          )))
|#

;220617 fixed the first note
(defun accent-apply-sl (inote ext-left ext-right peak curve-left curve-right)
  ;(print-ll "accent-apply-sl input args: inote " inote " ext-left " ext-left " ext-right " ext-right " peak " peak " curve-left " curve-left " curve-right " curve-right)
  (let ((istart (if (float ext-left)
                    (i?ndr-before-index inote ext-left)
                  (max (- inote ext-left) 0) ))
        (iend (if (float ext-right)
                  (i?ndr-after-index inote ext-right)
                (min (+ inote ext-left) (i?last)) ))
        fun-left fun-right power-left power-right )
    ;translate from keywords to function names and power
    ;(print-ll "istart " istart " inote " inote " iend " iend)
    (multiple-value-setq (fun-left power-left) (accents-translate-curv-name-left curve-left))
    (multiple-value-setq  (fun-right power-right) (accents-translate-curv-name-right curve-right))
    (cond ((= istart iend)               ;check if seg has 0 length then just set the current note (can happen for first note with small width)
           (iset inote 'dsl-temp peak) )
          (t                             ;otherwise apply the curves
           (iset-ramp-x2-decimal-last istart inote 0.0 0.0 0.0 peak 'dsl-temp power-left fun-left fun-left)
           (iset-ramp-x2-decimal-last inote iend 0.0 0.0 peak 0.0 'dsl-temp power-right fun-right fun-right) ))
    ;(print-ll "dsl " (iget inote 'dsl-temp))
    ;transfer dsl-temp to dsl using max
    (loop for i from istart to iend do
          (if (iget i 'sl) (iset i  'dsl (max (iget i 'dsl-temp) (or (iget i 'dsl) 0))))
          (rem-var (nth i *v*) 'dsl-temp)
          )))

;; same for dr
;; 120327/af
;; 220615 updated inote as above
;; 220616 skipped the max value and changed back to normal additive function
#|
(defun accent-apply-dr (inote ext-left ext-right peak curve-left curve-right)
  (let ((istart (if (float ext-left)
                   (i?ndr-before-index inote ext-left)
                  (max (- inote ext-left) 0) ))
        (iend (if (float ext-right)
                   (i?ndr-after-index inote ext-right)
                (min (+ inote ext-left) (i?last)) ))
        fun-left fun-right power-left power-right )
    ;translate from keywords to function names and power
    (multiple-value-setq (fun-left power-left) (accents-translate-curv-name-left curve-left))
    (multiple-value-setq  (fun-right power-right) (accents-translate-curv-name-right curve-right))
    ;(print-ll " istart " istart " inote " inote)
    (iset-ramp-x2-decimal-last istart inote 0.0 0.0 0.0 peak 'ddr power-left fun-left fun-left)
    (iset-ramp-x2-decimal-last inote iend 0.0 0.0 peak 0.0 'ddr power-right fun-right fun-right)
    (loop for i from istart to iend do
        ; (iset i  'dr (max (iget i 'dr) (* (iget i 'ndr) (1+ (iget i 'ddr))))) ;skipped max
        (iset i  'dr (* (iget i 'dr) (1+ (iget i 'ddr))))
        (rem-var (nth i *v*) 'ddr)
        )))
|#

#|
(defun accent-apply-dr (inote ext-left ext-right peak curve-left curve-right)
  (let ((istart (if (float ext-left)
                   (i?ndr-before-index inote ext-left)
                  (max (- inote ext-left) 0) ))
        (iend (if (float ext-right)
                   (i?ndr-after-index inote ext-right)
                (min (+ inote ext-left) (i?last)) ))
        fun-left fun-right power-left power-right )
    ;translate from keywords to function names and power
    (multiple-value-setq (fun-left power-left) (accents-translate-curv-name-left curve-left))
    (multiple-value-setq  (fun-right power-right) (accents-translate-curv-name-right curve-right))
    ;(print-ll " istart " istart " inote " inote  " iend " iend)
    (iset-ramp-x2-decimal-last istart inote 0.0 0.0 0.0 peak 'ddr-temp power-left fun-left fun-left)
    (iset-ramp-x2-decimal-last inote iend 0.0 0.0 peak 0.0 'ddr-temp power-right fun-right fun-right)
    (loop for i from istart to iend do
        (iset i 'ddr (max (iget i 'ddr-temp) (or (iget i 'ddr) 0))) 
        (rem-var (nth i *v*) 'ddr-temp)
        )))
|#

;220617 fixed the first note
(defun accent-apply-dr (inote ext-left ext-right peak curve-left curve-right)
  (let ((istart (if (float ext-left)
                    (i?ndr-before-index inote ext-left)
                  (max (- inote ext-left) 0) ))
        (iend (if (float ext-right)
                  (i?ndr-after-index inote ext-right)
                (min (+ inote ext-left) (i?last)) ))
        fun-left fun-right power-left power-right )
    ;translate from keywords to function names and power
    (multiple-value-setq (fun-left power-left) (accents-translate-curv-name-left curve-left))
    (multiple-value-setq  (fun-right power-right) (accents-translate-curv-name-right curve-right))
    ;(print-ll " istart " istart " inote " inote  " iend " iend)
    (cond ((= istart iend)               ;check if seg has 0 length then just set the current note (can happen for first note with small width)
           (iset inote 'ddr-temp peak) )
          (t                             ;otherwise apply the curves
           (iset-ramp-x2-decimal-last istart inote 0.0 0.0 0.0 peak 'ddr-temp power-left fun-left fun-left)
           (iset-ramp-x2-decimal-last inote iend 0.0 0.0 peak 0.0 'ddr-temp power-right fun-right fun-right) ))
    (loop for i from istart to iend do
          (iset i 'ddr (max (iget i 'ddr-temp) (or (iget i 'ddr) 0))) 
          (rem-var (nth i *v*) 'ddr-temp)
          )))


;;----utility functions---------------

;;translates the names from the drop down menu to function names and power values
(defun accents-translate-curv-name-right (keyword)
  (let (function (power 0))
    (when (equal :linear keyword) (setq function 'power-fn-acc) (setq power 1))
    (when (equal :quadratic keyword) (setq function 'power-fn-acc) (setq power 2))
    (when (equal :cubic keyword) (setq function 'power-fn-acc) (setq power 3))
    (when (equal :exponential keyword) (setq function 'accum-fn-acc) (setq power 1))
    (when (equal :cosine keyword) (setq function 'cos-fn-acc))
    (when (equal :gaussian keyword) (setq function 'gauss-fn-acc))
    (when (equal :hand-gesture keyword) (setq function 'hand-gesture-fn-acc))
    (values function power)
    ))

(defun accents-translate-curv-name-left (keyword)
  (let (function (power 0))
    (when (equal :linear keyword) (setq function 'power-fn-dec) (setq power 1))
    (when (equal :quadratic keyword) (setq function 'power-fn-dec) (setq power 2))
    (when (equal :cubic keyword) (setq function 'power-fn-dec) (setq power 3))
    (when (equal :exponential keyword) (setq function 'accum-fn-dec) (setq power 1))
    (when (equal :cosine keyword) (setq function 'cos-fn-dec))
    (when (equal :gaussian keyword) (setq function 'gauss-fn-dec))
    (when (equal :hand-gesture keyword) (setq function 'hand-gesture-fn-dec))
    (values function power)
    ))

;; get index ndrtarget ms before the note iend
;; returns index and remaining ndr
;; if it reach the beginning it returns the start pos (0 0)
(defun i?ndr-before-index (ipos ndrtarget)
  (let ((i ipos) (ndrtot 0))
    (while (and (< ndrtot ndrtarget) (> i 0))
      (decf i)
      (incf ndrtot (iget i 'ndr)) )
      (values i (if (<= (- ndrtot ndrtarget) 0) 0 (- ndrtot ndrtarget)))
    ))

;;same after
;; if it reach the end it returns the last pos (<last index> <ndr last note>)
(defun i?ndr-after-index (ipos ndrtarget)
  (let ((i ipos) (ndrtot 0) (ilast (i?last)))
    (while (and (< (+ ndrtot (iget i 'ndr)) ndrtarget) (< i ilast))
      (incf ndrtot (iget i 'ndr))
      (incf i)
      )
    (values i (if (= i ilast) 
                  (min (iget i 'ndr) (- ndrtarget ndrtot))
                (- ndrtarget ndrtot)
                ))
    ))


;;FUNCTIONS used for the shape of accents:

;;----power function
;; defined in phrase-arch
#|
(defun power-fn-dec (x power)
  (expt x power))
(defun power-fn-acc (x power)
  (abs (expt (- x 1) power)))
|#


;;----gaussian
(defun gauss-fn-dec (x power)
  (exp (/ (expt (- x 1) 2) -0.1))
  )
(defun gauss-fn-acc (x power)
  (gauss-fn-dec (- 1 x) 0)
  )     

;;----cosinus
(defun cos-fn-dec (x power)
  (/ (+ (cos (- (* x pi) pi)) 1) 2.0)
  )
(defun cos-fn-acc (x power)
  (cos-fn-dec (- 1 x) 0)
  ) 

;;----exp1 
(defun accum-fn-dec (x power)
  (exp (/ (expt (- x 1) 1) 0.1))
  )
(defun accum-fn-acc (x power)
  (accum-fn-dec (- 1 x) 0)
  )

;;----exp2
(defun accumslow-fn-dec (x power)
  (exp (/ (expt (- x 1) 1) 1.5))
  )
(defun accumslow-fn-acc (x power)
  (accumslow-fn-dec (- 1 x) 0)
  )

;;----exp3
(defun accumfast-fn-dec (x power)
  (exp (/ (expt (- x 1) 1) 0.08))
  )
(defun accumfast-fn-acc (x power)
  (accumfast-fn-dec (- 1 x) 0)
  )



;;----------------end----------------------------




    

