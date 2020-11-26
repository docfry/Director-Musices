(in-package "DM")

;;created: 990526 Anders Friberg
;;000303/af added first-note-in-measure-amp
;;200310/af started to work on this again - almost exactly 20 years later as last comment!
;;200914 changed all rule names for tempo prop timing



;----------measure marking-----------------------------


;;Coda wish
(defun first-note-in-measure-amp (quant)
  (each-note-if
    (this 'bar)
    (not (this 'rest))
    (then
      (add-this 'sl
                (* quant 2) ))))


;;----------- inegales ---------------------------------------
;early version described in Friberg 1991 for example

;inegales weak eighth note onsets
;similar to old definition but applies only on eighth notes
;; gives a ratio of about 1.56 (1+0.22)/(1-0.22)
;; 200201/af added 50 ms lower limit on dr - should only be 
;; applied on beat/2 dr
;; define a new marker: :swingpoint
(defun inegales (quant)
  (let ((beat-dr))
    (mark-offbeat)
    (each-note
      (when (this 'mm)
        (setq beat-dr (/ 60000.0 (this 'mm)))
        ;;(print beat-dr)
        )
      
      (when (this :offbeat)
        (let ((addval (* (/ beat-dr 2.0) 0.22 quant)))
          (when (> (- (prev 'dr) addval) 50)
            (add-this 'dr (- addval))
            (add-prev 'dr addval)
            )))
      )
    (rem-all :offbeat)
    ))

;;----------- swing ---------------------------------------

#|
;simple version of swing with swing ratio as input parameter
(defun swing (swing-ratio)
(let ((beat-dr)
(dr-percent (/ (- swing-ratio 1.0) (+ swing-ratio 1.0)))
)
(mark-offbeat)
(each-note
(when (this 'mm)
(setq beat-dr (/ 60000.0 (this 'mm)))
(print beat-dr)
)

(when (this :offbeat)
(let ((addval (* (/ beat-dr 2.0) dr-percent)))
(add-this 'dr (- addval))
(add-prev 'dr addval)
))
)
(rem-all :offbeat)
))

(defun swing (swing-ratio &key trackname)
(let ((beat-dr)
(dr-percent (/ (- swing-ratio 1.0) (+ swing-ratio 1.0)))
)
(mark-offbeat)
(each-track
(when (or (not trackname)
(string-equal (get-track-var 'trackname) trackname) )
(each-note
(when (this 'mm)
(setq beat-dr (/ 60000.0 (this 'mm)))
(print beat-dr)
)
(when (this :offbeat)
(let ((addval (* (/ beat-dr 2.0) dr-percent)))
(add-this 'dr (- addval))
(add-prev 'dr addval)
))
)))
(rem-all :offbeat)
))
|#
;with track delay key parameter
(defun swing (swing-ratio &key trackname (delay 0.0))
  (let ((beat-dr)
        (dr-percent (/ (- swing-ratio 1.0) (+ swing-ratio 1.0)))
        addval )
    (mark-offbeat)
    (each-track
      (when (or (not trackname)
                (string-equal (get-track-var 'trackname) trackname) )
        (each-note
          (when (first?)
            (add-this 'dr delay) )  ;track delay
          (when (this 'mm)
            (setq beat-dr (/ 60000.0 (this 'mm)))
            (setq addval (* (/ beat-dr 2.0) dr-percent)) )
          (when (this :offbeat)
            (add-this 'dr (- addval))
            (add-prev 'dr addval)
            ))))
    (rem-all :offbeat)
    ))

;-----------------------------------------------------
;---- new way of changing the swing prop to tempo-----
;using the linear interpolations from Friberg&Sundstrom 2002
;200422/af started
;-----------------------------------------------------

; apply swing ratio proportional to tempo with drums setting 
#|
(defun swing-tempo-prop-all (quant)
  (let ((beat-dr)
        (swing-ratio)
        (dr-percent )
        )
    (mark-offbeat)
    (each-note
      (when (this 'mm)
        (setq beat-dr (/ 60000.0 (this 'mm)))
        ;(print beat-dr)
        (setq swing-ratio (- 4.9396 (* 0.0124 (this 'mm))))
        (setq dr-percent (/ (- swing-ratio 1.0) (+ swing-ratio 1.0)))
        )
      (when (this :offbeat)
        (let ((addval (* (/ beat-dr 2.0) dr-percent quant)))
          (add-this 'dr (- addval))
          (add-prev 'dr addval)
          ))
      )
    (rem-all :offbeat)
    ))
    |#

(defun swing-ratio-tempo-prop-all-old (quant)
  (let ((beat-dr)
        (swing-ratio)
        (dr-percent )
        )
    (mark-offbeat)
    (each-note
      (when (this 'mm)
        (setq beat-dr (/ 60000.0 (this 'mm)))
        ;(print beat-dr)
        (setq swing-ratio (- 4.9396 (* 0.0124 (this 'mm))))
        (setq dr-percent (/ (- swing-ratio 1.0) (+ swing-ratio 1.0)))
        )
      (when (and (this :offbeat) 
                 (not (first?))
                 (>= (get-note-value-fraction *i*) 1/8) ;only on eight notes or longer
                 (>= (get-note-value-fraction (- *i* 1)) 1/8) ) ; also prev note
        (let ((addval (* (/ beat-dr 2.0) dr-percent quant)))
          (add-this 'dr (- addval))
          (add-prev 'dr addval)
          ))
      )
    (rem-all :offbeat)
    ))

; new version according to 2020 paper
; 200925 corrected a bug - minus sign added to slope
#|
(defun swing-ratio-tempo-prop-all (quant)
  (let ((beat-dr)
        (swing-ratio)
        (dr-percent) )
    (mark-offbeat)
    (each-note
      (when (this 'mm)
        (setq beat-dr (/ 60000.0 (this 'mm)))
        ;(print beat-dr)
        (setq swing-ratio (+ (* -0.01 (this 'mm)) 4.1019))
        (if (> swing-ratio 3.1019) (setq swing-ratio 3.1019)) ;boundaries according to 2020 paper
        (if (< swing-ratio 1) (setq swing-ratio 1))
        (setq dr-percent (/ (- swing-ratio 1.0) (+ swing-ratio 1.0))) )
      (when (and (this :offbeat) 
                 (not (first?))
                 (>= (get-note-value-fraction *i*) 1/8) ;only on eight notes or longer
                 (>= (get-note-value-fraction (- *i* 1)) 1/8) ) ; also prev note
        (let ((addval (* (/ beat-dr 2.0) dr-percent quant)))
          (add-this 'dr (- addval))
          (add-prev 'dr addval)
          ))
      )
    (rem-all :offbeat)
    ))
    |#
    
;added keywords
;skipped lower boundary
#|(defun swing-ratio-tempo-prop-all (quant &key (slope -0.01) (constant 4.1019))
  (let ((beat-dr)
        (swing-ratio)
        (dr-percent) )
    (mark-offbeat)
    (each-note
      (when (this 'mm)
        (setq beat-dr (/ 60000.0 (this 'mm)))
        ;(print beat-dr)
        (setq swing-ratio (+ (* slope (this 'mm)) constant))
        ;(print-ll "swing ratio=" swing-ratio " mm=" (this 'mm) " slope=" slope " constant=" constant)
        ;(if (<= (this 'mm) 100) (setq swing-ratio (+ (* slope 100) constant))) ;boundaries according to 2020 paper
        (if (< swing-ratio 1) (setq swing-ratio 1))
        (print-ll "swing-ratio-tempo-prop-all: final swing ratio = " swing-ratio)
        (setq dr-percent (/ (- swing-ratio 1.0) (+ swing-ratio 1.0))) )
      (when (and (this :offbeat) 
                 (not (first?))
                 (>= (get-note-value-fraction *i*) 1/8) ;only on eight notes or longer
                 (>= (get-note-value-fraction (- *i* 1)) 1/8) ) ; also prev note
        (let ((addval (* (/ beat-dr 2.0) dr-percent quant)))
          (add-this 'dr (- addval))
          (add-prev 'dr addval)
          ))
      )
    (rem-all :offbeat)
    ))
|#
; redefined according to paper
(defun swing-ratio-tempo-prop-all (k &key (slope -0.01) (constant 4.1019))
  (let ((beat-dr)
        (swing-ratio) )
    (mark-offbeat)
    (each-note
      (when (this 'mm)
        (setq beat-dr (/ 60000.0 (this 'mm)))
        (setq swing-ratio (+ (* slope (this 'mm)) constant))
        ;(if (<= (this 'mm) 100) (setq swing-ratio (+ (* slope 100) constant))) ;boundaries according to 2020 paper
        (if (< swing-ratio 1) (setq swing-ratio 1))
        (setq deltat (* k (* (/ beat-dr 2.0) (/ (- swing-ratio 1.0) (+ swing-ratio 1.0)))))
        (print-ll "swing-ratio-tempo-prop-all: final swing ratio (k=1) = " swing-ratio " deltat (resulting value) = " deltat)
	     )
      (when (and (this :offbeat) 
                 (not (first?))
                 ;(>= (get-note-value-fraction *i*) 1/8) ;only on eight notes or longer
                 (> (get-note-value-fraction *i*) 1/16) ;only on longer than sixteens notes
                 (>= (get-note-value-fraction (- *i* 1)) 1/8) ) ; also prev note
          (add-this 'dr (- deltat))
          (add-prev 'dr deltat)
          )
      )
    (rem-all :offbeat)
    ))


;delay amount according to soloist regression line in paper with quant = 1
;restrict to only 1/8 notes or longer!!!
;only eight notes or longer are affected - other note values need to be modeled separately
#|
(defun swing-tempo-prop-beat-delay-solo (quant &key trackname)
  (let ((beat-delay))
    (mark-beat)
    (each-track
      (when (and trackname
                 (string-equal (get-track-var 'trackname) trackname) )
        (each-note
          (when (this 'mm)
            (setq beat-delay (* quant (- 95.68 (* 0.2718 (this 'mm)))))  ;beat delay
            )
          (when (and (this :beat) 
                     (not (first?))
                     (>= (get-note-value-fraction *i*) 1/8) ;only on eight notes or longer
                     (>= (get-note-value-fraction (- *i* 1)) 1/8) ) ; also prev note
            (add-this 'dr (- beat-delay))
            (add-prev 'dr beat-delay)
            ))
        ))
    (rem-all :beat)
    ))
    |#

;new method preserving all triplets and 16ths etc.
;Assume that the drum swing (swing-tempo-prop-all) is applied to everything as before
;Step 1. Delay the whole track with the beat delay
;Step 2.  Move all eight note offsets back to the beat delay
(defun swing-beat-delay-tempo-prop-solo-old (quant &key trackname)
  (let ((beat-delay))
    (mark-offbeat)
    (each-track
      (when (and trackname
                 (string-equal (get-track-var 'trackname) trackname) )
        (each-note
          (when (this 'mm)
            (setq beat-delay (* quant (- 95.68 (* 0.2718 (this 'mm)))))  ;beat delay
            (add-this 'dr beat-delay) ;step 1
            )
          (when (and (this :offbeat) 
                     (not (first?))
                     (>= (get-note-value-fraction *i*) 1/8) ;only on eight notes or longer
                     (>= (get-note-value-fraction (- *i* 1)) 1/8) ) ; also prev note
            (add-this 'dr beat-delay)
            (add-prev 'dr (- beat-delay))
            ))
        ))
    (rem-all :offbeat)
    ))

;new version following the description in the 2020 paper
#|
(defun swing-beat-delay-tempo-prop-solo (quant &key trackname)
  (let ((beat-delay))
    (mark-offbeat)
    (each-track
      (when (and trackname
                 (string-equal (get-track-var 'trackname) trackname) )
        (each-note
          (when (this 'mm)
            (setq beat-delay (* quant (- 107.17 (* 0.3078 (this 'mm)))))  ;beat delay
            (add-this 'dr beat-delay) ;step 1
            )
          (when (and (this :offbeat) 
                     (not (first?))
                     (>= (get-note-value-fraction *i*) 1/8) ;only on eight notes or longer
                     (>= (get-note-value-fraction (- *i* 1)) 1/8) ) ; also prev note
            (add-this 'dr beat-delay)
            (add-prev 'dr (- beat-delay))
            ))
        ))
    (rem-all :offbeat)
    ))
    |#
;added keyword parameters for linear regression
;201126/af changed regression parameters
(defun swing-beat-delay-tempo-prop-solo (k &key trackname (slope -0.4101) (constant 125.1))
  (let ((beat-delay))
    (mark-offbeat)
    (each-track
      (when (and trackname
                 (string-equal (get-track-var 'trackname) trackname) )
        (each-note
          (when (this 'mm)
            (setq beat-delay (* k (+ (* slope (this 'mm)) constant)))  ;beat delay
            (add-this 'dr beat-delay) ;step 1 -assumes a tempo mark only on first note!
            (print-ll "swing-beat-delay-tempo-prop-solo: beat delay = " beat-delay)
            )
          (when (and (this :offbeat) 
                     (not (first?))
                     (> (get-note-value-fraction *i*) 1/16) ;only on longer than sixteens notes
                     (>= (get-note-value-fraction (- *i* 1)) 1/8) ) ; also prev note
            (add-this 'dr beat-delay)
            (add-prev 'dr (- beat-delay))
            ))
        ))
    (rem-all :offbeat)
    ))

;delay amount according to bass measurements (?)
(defun swing-beat-delay-tempo-prop-bass-old (quant &key trackname)
  (let ((beat-delay))
    (mark-beat)
    (each-track
      (when (and trackname
                 (string-equal (get-track-var 'trackname) trackname) )
        (each-note
          (when (this 'mm)
            (setq beat-delay (* quant (- 31.242 (* 0.057 (this 'mm)))))  ;beat delay from where?
            )
          (when (and (this :beat) (not (first?)))
            (add-this 'dr (- beat-delay))
            (add-prev 'dr beat-delay)
            ))
        ))
    (rem-all :beat)
    ))

;new version according to paper
;reformulated as the solo track
#|
(defun swing-beat-delay-tempo-prop-bass (quant &key trackname)
  (let ((beat-delay))
    (mark-offbeat)
    (each-track
      (when (and trackname
                 (string-equal (get-track-var 'trackname) trackname) )
        (each-note
          (when (this 'mm)
            (setq beat-delay (* quant (- 23.279 (* 0.0536 (this 'mm)))))  ;beat delay
            (add-this 'dr beat-delay) ;step 1
            )
          (when (and (this :offbeat) 
                     (not (first?))
                     (>= (get-note-value-fraction *i*) 1/8) ;only on eight notes or longer
                     (>= (get-note-value-fraction (- *i* 1)) 1/8) ) ; also prev note
            (add-this 'dr beat-delay)
            (add-prev 'dr (- beat-delay))
            ))
        ))
    (rem-all :offbeat)
    ))
    |#
;with regression parameters
(defun swing-beat-delay-tempo-prop-bass (k &key trackname (slope -0.0536) (constant 23.279))
  (let ((beat-delay))
    (mark-offbeat)
    (each-track
      (when (and trackname
                 (string-equal (get-track-var 'trackname) trackname) )
        (each-note
          (when (this 'mm)
            (setq beat-delay (* k (+ (* slope (this 'mm)) constant)))  ;beat delay
            (print-ll "swing-beat-delay-tempo-prop-bass: beat delay = " beat-delay)
            (add-this 'dr beat-delay) ;step 1
            )
          (when (and (this :offbeat) 
                     (not (first?))
                     (> (get-note-value-fraction *i*) 1/16) ;only on longer than sixteens notes
                     (>= (get-note-value-fraction (- *i* 1)) 1/8) ) ; also prev note
            (add-this 'dr beat-delay)
            (add-prev 'dr (- beat-delay))
            ))
        ))
    (rem-all :offbeat)
    ))

;just adds a delay in ms to the first note of the track
;for testing
(defun swing-simple-delay-one-track-ms (ms &key trackname)
  (each-track
    (when (and trackname
               (string-equal (get-track-var 'trackname) trackname) )
      (each-note-if
        (first?)
        (then
          (add-this 'dr ms) 
          )))))

;adds a delay in ms to all offbeat eight notes
;trackname mandatory
(defun swing-offbeat-delay-one-track-ms (offbeat-delay &key trackname)
    (mark-offbeat)
    (each-track
      (when (and trackname
                 (string-equal (get-track-var 'trackname) trackname) )
        (each-note
          (when (and (this :offbeat) 
                     (not (first?))
                     (> (get-note-value-fraction *i*) 1/16) ;only on longer than sixteens notes
                     (>= (get-note-value-fraction (- *i* 1)) 1/8) ) ; also prev note
            (add-this 'dr (- offbeat-delay))
            (add-prev 'dr offbeat-delay)
            ))
        ))
    (rem-all :offbeat)
    )


;adds a SL accent on every 8th note offbeat 
(defun swing-offbeat-accent (quant &key trackname)
  (mark-offbeat)
  (each-track
    (when (and trackname
               (string-equal (get-track-var 'trackname) trackname) )
      (each-note-if
        (not (this 'rest))
        (this :offbeat)
        (= (get-note-value-fraction *i*) 1/8) ;only on eight notes
        (then
         (add-this 'sl (* quant 3.0))
          ))))
  (rem-all :offbeat)
  )

;----------------------------------------------------------
;- applying the rules and generate midi files for paper----
;----------------------------------------------------------



;----------------------------------------------------------
;---- original way of changing the swing prop to tempo-----
;----------------------------------------------------------
#|
(defun swing-tempo-prop-drums (quant &key trackname)
  (let ((beat-dr)
        (swing-ratio)
        (dr-percent )
        )
    (mark-offbeat)
    (each-track
      (when (and trackname
                 (string-equal (get-track-var 'trackname) trackname) )
        (each-note
          (when (this 'mm)
            (setq beat-dr (/ 60000.0 (this 'mm)))
            ;(print beat-dr)
            (setq swing-ratio (- 4.9396 (* 0.0124 (this 'mm))))
            (setq dr-percent (/ (- swing-ratio 1.0) (+ swing-ratio 1.0)))
            )
          (when (this :offbeat)
            (let ((addval (* (/ beat-dr 2.0) dr-percent quant)))
              (add-this 'dr (- addval))
              (add-prev 'dr addval)
              ))
          )))
    (rem-all :offbeat)
    ))

(defun swing-tempo-prop-drums-amp (quant &key trackname)
  (mark-offbeat)
  (each-track
    (when (and trackname
               (string-equal (get-track-var 'trackname) trackname) )
      (each-note
        (when (this :offbeat)
          (add-this 'sl (* quant -3.0))
          ))
      ))
  (rem-all :offbeat)
  )

(defun swing-tempo-prop-soloist (quant &key trackname)
  (let ((beat-dr)
        (swing-ratio)
        (dr-percent )
        )
    (mark-offbeat)
    (each-track
      (when (and trackname
                 (string-equal (get-track-var 'trackname) trackname) )
        (each-note
          (when (this 'mm)
            (setq beat-dr (/ 60000.0 (this 'mm)))
            ;(print beat-dr)
            (setq swing-ratio (- 2.0483 (* 0.0031 (this 'mm))))
            (setq dr-percent (/ (- swing-ratio 1.0) (+ swing-ratio 1.0)))
            (add-this 'dr (- 95.68 (* 0.2718 (this 'mm))))  ;beat delay
            )
          (when (this :offbeat)
            (let ((addval (* (/ beat-dr 2.0) dr-percent quant)))
              (add-this 'dr (- addval))
              (add-prev 'dr addval)
              ))
          )))
    (rem-all :offbeat)
    ))

;200312/af will not generate the right offset sync for eight notes since the swing ratio is same as for soloist
(defun swing-tempo-prop-bass (quant &key trackname)
  (let ((beat-dr)
        (swing-ratio)
        (dr-percent )
        )
    (mark-offbeat)
    (each-track
      (when (and trackname
                 (string-equal (get-track-var 'trackname) trackname) )
        (each-note
          (when (this 'mm)
            (setq beat-dr (/ 60000.0 (this 'mm)))
            ;(print beat-dr)
            (setq swing-ratio (- 2.0483 (* 0.0031 (this 'mm)))) ;same as soloist
            (setq dr-percent (/ (- swing-ratio 1.0) (+ swing-ratio 1.0)))
            (add-this 'dr (- 31.242 (* 0.057 (this 'mm))))  ;beat delay from where?
            )
          (when (this :offbeat)
            (let ((addval (* (/ beat-dr 2.0) dr-percent quant)))
              (add-this 'dr (- addval))
              (add-prev 'dr addval)
              ))
          )))
    (rem-all :offbeat)
    ))

;combine all three
;Use "no sync" option in rule palette
(defun ensemble-swing (quant &key drums solo bass)
  (swing-tempo-prop-drums quant :trackname drums)
  (swing-tempo-prop-soloist quant :trackname solo)
  (swing-tempo-prop-bass quant :trackname bass)
  )
  
|#

;increase sl at offbeats
;k=1 means 3dB increase
(defun offbeat-sl (k &key trackname)
  (mark-offbeat)
  (each-track
    (when (or (not trackname)
              (and trackname (string-equal (get-track-var 'trackname) trackname) ))
      (each-note-if
        (this :offbeat)
        (then
          (add-this 'sl (* k 3)) ))))
  (rem-all :offbeat) )


;;if quarter on beat 1 or 3 - the following note on the beat 2 or 4 gets an accent
;;in eighth notes the offbeat gets an accent
(defun swing-accents-sl (quant)
  (mark-offbeat)
  (mark-beat-number-in-measure)
  ;;---accent on 2 and 4-------
  (each-note-if
    (not (last?))
    (not (next 'rest))
    (this :beat)
    (or (= (this :beat) 1) (= (this :beat) 3))
    (= 4 (note-to-notevalue (this 'n)))
    (then 
      (set-next 'dsl (* quant 3)) )) ;3 dB default
  ;;---accent on "weak" eighths---
  (each-note-if 
    (this :offbeat)
    (not (first?))
    (not (prev 'dsl))
    (= 8 (note-to-notevalue (prev 'n)))
    (= 8 (note-to-notevalue (this 'n)))
    (not (or (this 'bind) (this 'tie)))
    (then 
      (set-this 'dsl (* quant 3)) )) ;3 dB default
  
  (each-note-if
    (this 'dsl)
    (then
      (add-this 'sl (this 'dsl))
      (rem-this 'dsl) ))
  
  (rem-all :offbeat)
  (rem-all :beat)
  )

;;; ---------------- articulation of beat notes---------------------

;;staccato on quarter notes
;; quant=1 -> 50%
(defun quarter-note-art (quant &key trackname)
  (each-track
    (when (or (not trackname)
              (and trackname (string-equal (get-track-var 'trackname) trackname) ))
      (each-note-if
        (not (this 'rest))
        (not (this 'dot))
        (not (this 'tuple))
        (or (and (numberp (note-to-notevalue (this 'n))) ;old type note value
                 (= 4 (note-to-notevalue (this 'n))) )
            (and (not (numberp (note-to-notevalue (this 'n))))
                 (= 1 (length (note-to-notevalue (this 'n)))) ;midi type note value
                 (= 1/4 (car (note-to-notevalue (this 'n)))) ))
        (then
          (if (this 'dro)
              (add-this 'dro (* (this 'dr) quant 0.5))
              (set-this 'dro (* (this 'dr) quant 0.5))
              ))))))

#|
(defun beat-note-art (quant &key trackname)
(let ((beat 4))
(each-track
(when (or (not trackname)
(and trackname (string-equal (get-track-var 'trackname) trackname) ))
(each-note
(if (this 'meter) (setq beat (cadr (this 'meter))))
(cond ((and 
(not (this 'rest))
(not (this 'dot))
(not (this 'tuple))
(or (and (numberp (note-to-notevalue (this 'n))) ;old type note value
(= beat (note-to-notevalue (this 'n))) )
(and (not (numberp (note-to-notevalue (this 'n))))
(= 1 (length (note-to-notevalue (this 'n)))) ;midi type note value
(= (/ 1 beat) (car (note-to-notevalue (this 'n)))) )
)
)
(if (this 'dro)
(add-this 'dro (* (this 'dr) quant 0.3))
(set-this 'dro (* (this 'dr) quant 0.3)) )
)))))))
|#

;;generalized version looking at the meter
;;not on step-wise melodies
(defun beat-note-art (quant &key trackname)
  (let ((beat 4))
    (each-track
      (when (or (not trackname)
                (and trackname (string-equal (get-track-var 'trackname) trackname) ))
        (each-note-if
          (not (this 'rest))
          (not (this 'dot))
          (not (this 'tuple))
          (not (last?))
          (and (not (next 'rest))
               (> (abs (- (this 'f0) (next 'f0))) 2)) ;not on step-wise motion to next note
          (then 
            (if (this 'meter) (setq beat (cadr (this 'meter))))
            (if (or (and (numberp (note-to-notevalue (this 'n))) ;old type note value
                         (= beat (note-to-notevalue (this 'n))) )
                    (and (not (numberp (note-to-notevalue (this 'n))))
                         (= 1 (length (note-to-notevalue (this 'n)))) ;midi type note value
                         (= (/ 1 beat) (car (note-to-notevalue (this 'n)))) )
                    )
                (if (this 'dro)
                    (add-this 'dro (* (this 'dr) quant 0.3))
                    (set-this 'dro (* (this 'dr) quant 0.3)) )
                )))))))



;;;;;-------------

;dro = k*quant*log(dr)/f0

;; interpolate effect between 100 and 500 ms IOI
;; 0 below and 0.25 percent above
(defun long-staccato (quant)
  (each-note-if
    (not (last?))
    (not (this 'rest))
    (not (next 'rest))
    (> (this 'dr) 100)
    (then
      (let* ((dr (this 'dr))
             (k (infix (dr / 400.0 - 0.25))) 
             )
        (if (> k 1) (setq k 1))
        (if (this 'dro)
            (add-this 'dro (* k quant (this 'dr) 0.25))
            (set-this 'dro (* k quant (this 'dr) 0.25))
            )))))

;;;--------------------------------------
;;;try some simple beat salience patterns over bars
;;;0912/af
;;tested with all 52 ref mel with ok results
;;;--------------------------------------

;max sl add 1.5 dB for quant = 1
(defun beat-salience-accents-sl (quant)
  (setq quant (/ quant 2.0))  ; scaling of quant!
  (let (meter)
    (mark-beat-number-in-measure)
    ;;---accent on 2 and 4-------
    (each-note
      (when (this 'meter) (setq meter (this 'meter)) )
      (cond
        ((or (equal meter '(2 2)) (equal meter '(2 4)))
         (when (and (this :beat) (this 'sl) (= (this :beat) 1))
           (add-this 'sl (* quant 3)) )
         (when (and (this :beat) (this 'sl) (= (this :beat) 2))
           (add-this 'sl (* quant 2)) )
         )
        ((equal meter '(4 4))
         (when (and (this :beat) (this 'sl) (= (this :beat) 1))
           (add-this 'sl (* quant 3)) )
         (when (and (this :beat) (this 'sl) (= (this :beat) 2))
           (add-this 'sl (* quant 1)) )
         (when (and (this :beat) (this 'sl) (= (this :beat) 3))
           (add-this 'sl (* quant 2)) )
         (when (and (this :beat) (this 'sl) (= (this :beat) 4))
           (add-this 'sl (* quant 1)) )
         )
        ((or (equal meter '(3 4)) (equal meter '(3 8)))
         (when (and (this :beat) (this 'sl) (= (this :beat) 1))
           (add-this 'sl (* quant 3)) )
         (when (and (this :beat) (this 'sl) (= (this :beat) 2))
           (add-this 'sl (* quant 1)) )
         (when (and (this :beat) (this 'sl) (= (this :beat) 3))
           (add-this 'sl (* quant 1)) )
         )
        ((equal meter '(6 8))
         (when (and (this :beat) (this 'sl) (= (this :beat) 1))
           (add-this 'sl (* quant 3)) )
         (when (and (this :beat) (this 'sl) (= (this :beat) 4))
           (add-this 'sl (* quant 2)) )
         )
        )))
  (rem-all :beat) )



