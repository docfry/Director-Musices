;; Bisesi/Parncutt accent rules - Analysis of accent positions

;;130108/af started on metrical accent


(in-package :dm)

(defun mark-metrical-accent ()
  (rem-all :beat)
  (rem-all 'accent-m-auto)
  
  (mark-beat)
  
  (each-note-if
   (this :beat)
   (this 'bar)
   (not (this 'rest))
   (then
    (set-this 'accent-m-auto 4) ))
  
  (each-note-if
   (this :beat)
   (not (this 'bar))
   (not (this 'rest))
   (then
    ;(set-this 'accent-m-auto 0)
    ))
  )

(defun mark-metrical-accent ()
  (rem-all :beat0)(rem-all :beat1)(rem-all :beat2)(rem-all :beat3)
  (rem-all 'accent-m-auto)
  
  (mark-beat-levels)
  
  (each-note-if
   (not (this 'rest))
   (then
    (when (this :beat0) (set-this 'accent-m-auto nil))
    (when (this :beat1) (set-this 'accent-m-auto 4))
    (when (this :beat2) (set-this 'accent-m-auto 6))
    (when (this :beat3) (set-this 'accent-m-auto 6))
    )))

(defun mark-metrical-accent ()
  (rem-all :beat0)(rem-all :beat1)(rem-all :beat2)(rem-all :beat3)
  (rem-all :beat0sal)(rem-all :beat1sal)(rem-all :beat2sal)(rem-all :beat3sal)
  (rem-all 'accent-m-auto)
  ;mark metrical levels
  (mark-beat-levels)
  ;default saliences
  (each-note-if
   (not (this 'rest))
   (then
    (when (this :beat0) (set-this 'beat0sal 1.5))
    (when (this :beat1) (set-this 'beat1sal 2))
    (when (this :beat2) (set-this 'beat3sal 2.5))
    (when (this :beat3) (set-this 'beat4sal 3)) ))
  
    )

;mark 4 different metrical levels from notated meter
;beat0 sub-beat 
;beat1 the normal beat
;beat2 half bar or bar
;beat3 bar or 2 bars
(defun mark-beat-levels ()
  (let (beat0 beat1 beat2 beat3
              (ack-value 0.0) meter)
    (each-track
     (setq ack-value 0.0)
     (each-note             ;mark beat
      (when (this 'meter)
        (setq meter (this 'meter))
        (cond
         ((equal meter '(4 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(2 2)) (setq beat0 1/4 beat1 1/2 beat2 2/2 beat3 4/2))
         ((equal meter '(4 2)) (setq beat0 1/4 beat1 1/2 beat2 2/2 beat3 4/2))
         ((equal meter '(2 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(3 4)) (setq beat0 1/8 beat1 1/4 beat2 3/4 beat3 6/4))
         ((equal meter '(6 8)) (setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/8))
         ((equal meter '(9 8)) (setq beat0 1/8 beat1 3/8 beat2 9/8 beat3 18/8))
         ((equal meter '(12 8))(setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/8))
         ))
      ;(when (this 'bar) (setq ack-value 0))
      (when (zerop (mod ack-value beat0)) (set-this :beat0 t))
      (when (zerop (mod ack-value beat1)) (set-this :beat1 t))
      (when (zerop (mod ack-value beat2)) (set-this :beat2 t))
      (when (zerop (mod ack-value beat3)) (set-this :beat3 t))
      (incf ack-value (get-note-value-fraction *i*))
      ))))

