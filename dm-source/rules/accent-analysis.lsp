;; Bisesi/Parncutt immanent accent rules - Analysis of accent positions

;;130108/af started on metrical accent
;;170131/af added compensation for disklavier (bottom of file)
;;170725/af added more meters, no change to previous def of metric accent rule
;;210316/af checked the correspondence to the papers and clarified the comments
;;          metrical and melodic accent used in Bisesi-Friberg-Parncutt-2019
;;220620 Added a simple metrical accent, some cleaning

(in-package :dm)


;-----------------------------------------------------------------------
;-------------- metrical accent ----------------------------------------
;-----------------------------------------------------------------------

; main function used for immanent metrical accent model in Bisesi-Friberg-Parncutt-2019
; 4 different beat levels are defined
(defun mark-metrical-accent ()
  ;(rem-all :beat0)(rem-all :beat1)(rem-all :beat2)(rem-all :beat3)
  ;(rem-all :beat0sal)(rem-all :beat1sal)(rem-all :beat2sal)(rem-all :beat3sal)
  ;(rem-all :beat0dr)(rem-all :beat1dr)(rem-all :beat2dr)(rem-all :beat3dr)
  (rem-all 'accent-m)
  ;mark metrical levels
  (mark-beat-levels)
  ;default saliences
  (each-note-if
   (not (this 'rest))
   (then
    (when (this :beat0) (set-this :beat0sal 3))
    (when (this :beat1) (set-this :beat1sal 3))
    (when (this :beat2) (set-this :beat2sal 3))
    (when (this :beat3) (set-this :beat3sal 3)) ))
  ;compute pulse intervals
  (mark-beat-fraction-dr)
  ;gaussian scaling
  (scale-beat-salience)
  ;summarize into final accent salience
  (let ((beat0sal 0) (beat1sal 0) (beat2sal 0) (beat3sal 0))
    (each-note-if
     (not (this 'rest))
     ;(not (first?))  ;quickfix since it doesn't work to apply accent on the first note
     (then
      (setq beat0sal (or (this :beat0sal) 0))
      (setq beat1sal (or (this :beat1sal) 0))
      (setq beat2sal (or (this :beat2sal) 0))
      (setq beat3sal (or (this :beat3sal) 0))
      (set-this 'accent-m (* 0.3 (+ beat0sal beat1sal beat2sal beat3sal)))
      ;ERICA rescaled the salience
      )))
  (rem-all :beat0)(rem-all :beat1)(rem-all :beat2)(rem-all :beat3)
  (rem-all :beat0sal)(rem-all :beat1sal)(rem-all :beat2sal)(rem-all :beat3sal)
  (rem-all :beat0dr)(rem-all :beat1dr)(rem-all :beat2dr)(rem-all :beat3dr)
  )

;220620 A simple version in which accent salience of each metrical level is specified explicitly
; had to introduce the quant parameter for the rule palette
; it puts accent-m marks with the specified salience value in the score
; if nil is specified as the salience value, there will not be any mark at that level
(defun mark-metrical-accent-simple (quant &key (m0 0.5)(m1 1)(m2 1.5)(m3 2))
  (rem-all 'accent-m)
  ;mark metrical levels
  (mark-beat-levels)
  ;default saliences
  (each-note-if
    (not (this 'rest))
    (then
      (cond
       ((and (this :beat3) m3)
        (set-this 'accent-m (* quant m3)) )
       ((and (this :beat2) m2)
        (set-this 'accent-m (* quant m2)) )
       ((and (this :beat1) m1)
        (set-this 'accent-m (* quant m1)) )
       ((and (this :beat0) m0)
        (set-this 'accent-m (* quant m0)) )
       )))
  (rem-all :beat0)(rem-all :beat1)(rem-all :beat2)(rem-all :beat3)
  )

;mark 4 different metrical levels from notated meter
;beat0 sub-beat 
;beat1 the normal beat
;beat2 half bar or bar
;beat3 bar or 2 bars
;used in Friberg-Bisesi-Addessi-Baroni-2019 (see optimize-mel-accent-model-3.lsp)
(defun mark-beat-levels ()
  (let (beat0 beat1 beat2 beat3
              (ack-value 0.0) fractions)
    (each-track
     (setq ack-value 0)
     (each-note             ;mark beat
      (when (this 'meter)
        (setq fractions (get-beat-fractions (this 'meter)))
        (setq beat0 (first fractions))
        (setq beat1 (second fractions))
        (setq beat2 (third fractions))
        (setq beat3 (fourth fractions))
        (setq ack-value 0) ) ;170725 added, needed when meter change
      ;(when (this 'bar) (setq ack-value 0))
      (when (zerop (mod ack-value beat0)) (set-this :beat0 t))
      (when (zerop (mod ack-value beat1)) (set-this :beat1 t))
      (when (zerop (mod ack-value beat2)) (set-this :beat2 t))
      (when (zerop (mod ack-value beat3)) (set-this :beat3 t))
      ;(print-ll " mod beat1 " (mod ack-value beat1) " fraction " ack-value)
      (incf ack-value (get-note-value-fraction *i*))
      ))))

;list of translations from meter to beat level fractions
;used in Bisesi-Friberg-Parncutt-2019
#|
(defun get-beat-fractions (meter)
  (let ((beat0 1/8) (beat1 1/4)(beat2 2/4)(beat3 4/4)) ;default 4/4
        (cond
         ((equal meter '(4 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(2 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(2 2)) (setq beat0 1/4 beat1 1/2 beat2 2/2 beat3 4/2))
         ((equal meter '(4 2)) (setq beat0 1/4 beat1 1/2 beat2 2/2 beat3 4/2))
         ((equal meter '(3 4)) (setq beat0 1/8 beat1 1/4 beat2 3/4 beat3 6/4))
         ((equal meter '(3 8)) (setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/4))
         ((equal meter '(6 8)) (setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/8))
         ((equal meter '(9 8)) (setq beat0 1/8 beat1 3/8 beat2 9/8 beat3 18/8))
         ((equal meter '(12 8))(setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/8))
         (t (warn " get-beat-level-fractions, meter not defined: ~A ,using (4 4) instead" meter))
         )
        (list beat0 beat1 beat2 beat3)
        ))
|#

;170725 added meters for the 60 mel dataset, the old ones above unchanged
;compound meters are not fully defined since it requires a (manual) analysis of the score
;no contradictory cases found so far...
;Should be compatible with Bisesi-Friberg-Parncutt-2019
;used in Friberg-Bisesi-Addessi-Baroni-2019
#|
(defun get-beat-fractions (meter)
  (let ((beat0 1/8) (beat1 1/4)(beat2 2/4)(beat3 4/4)) ;default 4/4
        (cond
         ;2 or 4
         ((equal meter '(4 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(2 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(8 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(8 16)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(2 2)) (setq beat0 1/4 beat1 1/2 beat2 2/2 beat3 4/2))
         ((equal meter '(4 2)) (setq beat0 1/4 beat1 1/2 beat2 2/2 beat3 4/2))
         ;3,6,9...
         ((equal meter '(3 4)) (setq beat0 1/8 beat1 1/4 beat2 3/4 beat3 6/4))
         ((equal meter '(3 2)) (setq beat0 1/4 beat1 1/2 beat2 3/2 beat3 6/2)); no 36
         ((equal meter '(6 4)) (setq beat0 1/8 beat1 1/4 beat2 3/4 beat3 6/4))
         ((equal meter '(3 8)) (setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/4))
         ((equal meter '(6 8)) (setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/8))
         ((equal meter '(6 16)) (setq beat0 3/16 beat1 3/16 beat2 6/16 beat3 12/16)) ; no 55
         ((equal meter '(9 8)) (setq beat0 1/8 beat1 3/8 beat2 9/8 beat3 18/8))
         ((equal meter '(9 16)) (setq beat0 3/16 beat1 3/16 beat2 9/16 beat3 18/16)) ;no 53
         ((equal meter '(12 8))(setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/8))
         ((equal meter '(12 16)) (setq beat0 1/16 beat1 3/16 beat2 6/16 beat3 12/16)) ;no 53
         ;compound - different compromises
         ((equal meter '(5 4)) (setq beat0 1/8 beat1 1/4 beat2 5/4 beat3 5/4)) ;beat2 should be either at 2+3 or 3+2
         ((equal meter '(5 16)) (setq beat0 5/16 beat1 5/16 beat2 10/16 beat3 20/16)) ;no 53
         ((equal meter '(7 16)) (setq beat0 7/16 beat1 7/16 beat2 7/16 beat3 14/16)) ;beat0-1 not defined but set to higher level, no 30
         ((equal meter '(10 16)) (setq beat0 10/16 beat1 10/16 beat2 10/16 beat3 20/16)) ;beat0-1 not defined but set to higher level, no 55
         ((equal meter '(11 16)) (setq beat0 11/16 beat1 11/16 beat2 11/16 beat3 22/16)) ;beat0-1 not defined but set to higher level, no 30
         ((equal meter '(19 16)) (setq beat0 19/16 beat1 19/16 beat2 19/16 beat3 19/16)) ;beat0-2 not defined but set to higher level, no 30
         ((equal meter '(13 16)) (setq beat0 2/16 beat1 4/16 beat2 13/16 beat3 13/16)) ; no 53
         ((equal meter '(14 16)) (setq beat0 7/16 beat1 7/16 beat2 7/16 beat3 14/16)) ; no 53
         ((equal meter '(15 16)) (setq beat0 1/16 beat1 3/16 beat2 15/16 beat3 15/16)) ; no 53
         ((equal meter '(17 16)) (setq beat0 2/16 beat1 4/16 beat2 17/16 beat3 17/16)) ; no 53
         ;special
         ((equal meter '(25 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4)) ;no 21, as 4/4, maybe better to not define beat2-3
         ((equal meter '(34 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4)) ;no 24, as 4/4

         (t (warn " get-beat-level-fractions, meter not defined: ~A ,using (4 4) instead" meter))
         )
        (list beat0 beat1 beat2 beat3)
        ))
|#
;211011 Added 7/8 for Gabriel study
(defun get-beat-fractions (meter)
  (let ((beat0 1/8) (beat1 1/4)(beat2 2/4)(beat3 4/4)) ;default 4/4
        (cond
         ;2 or 4
         ((equal meter '(4 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(2 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(8 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(8 16)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4))
         ((equal meter '(2 2)) (setq beat0 1/4 beat1 1/2 beat2 2/2 beat3 4/2))
         ((equal meter '(4 2)) (setq beat0 1/4 beat1 1/2 beat2 2/2 beat3 4/2))
         ;3,6,9...
         ((equal meter '(3 4)) (setq beat0 1/8 beat1 1/4 beat2 3/4 beat3 6/4))
         ((equal meter '(3 2)) (setq beat0 1/4 beat1 1/2 beat2 3/2 beat3 6/2)); no 36
         ((equal meter '(6 4)) (setq beat0 1/8 beat1 1/4 beat2 3/4 beat3 6/4))
         ((equal meter '(3 8)) (setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/4))
         ((equal meter '(6 8)) (setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/8))
         ((equal meter '(6 16)) (setq beat0 3/16 beat1 3/16 beat2 6/16 beat3 12/16)) ; no 55
         ((equal meter '(9 8)) (setq beat0 1/8 beat1 3/8 beat2 9/8 beat3 18/8))
         ((equal meter '(9 16)) (setq beat0 3/16 beat1 3/16 beat2 9/16 beat3 18/16)) ;no 53
         ((equal meter '(12 8))(setq beat0 1/8 beat1 3/8 beat2 6/8 beat3 12/8))
         ((equal meter '(12 16)) (setq beat0 1/16 beat1 3/16 beat2 6/16 beat3 12/16)) ;no 53
         ;compound - different compromises
         ((equal meter '(5 4)) (setq beat0 1/8 beat1 1/4 beat2 5/4 beat3 5/4)) ;beat2 should be either at 2+3 or 3+2
         ((equal meter '(5 16)) (setq beat0 5/16 beat1 5/16 beat2 10/16 beat3 20/16)) ;no 53
         ((equal meter '(7 8)) (setq beat0 1/16 beat1 1/8 beat2 4/8 beat3 14/8)) ; no 25 new 30 set Gabriel
         ((equal meter '(7 16)) (setq beat0 7/16 beat1 7/16 beat2 7/16 beat3 14/16)) ;beat0-1 not defined but set to higher level, no 30
         ((equal meter '(10 16)) (setq beat0 10/16 beat1 10/16 beat2 10/16 beat3 20/16)) ;beat0-1 not defined but set to higher level, no 55
         ((equal meter '(11 16)) (setq beat0 11/16 beat1 11/16 beat2 11/16 beat3 22/16)) ;beat0-1 not defined but set to higher level, no 30
         ((equal meter '(19 16)) (setq beat0 19/16 beat1 19/16 beat2 19/16 beat3 19/16)) ;beat0-2 not defined but set to higher level, no 30
         ((equal meter '(13 16)) (setq beat0 2/16 beat1 4/16 beat2 13/16 beat3 13/16)) ; no 53
         ((equal meter '(14 16)) (setq beat0 7/16 beat1 7/16 beat2 7/16 beat3 14/16)) ; no 53
         ((equal meter '(15 16)) (setq beat0 1/16 beat1 3/16 beat2 15/16 beat3 15/16)) ; no 53
         ((equal meter '(17 16)) (setq beat0 2/16 beat1 4/16 beat2 17/16 beat3 17/16)) ; no 53
         ;special
         ((equal meter '(25 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4)) ;no 21, as 4/4, maybe better to not define beat2-3
         ((equal meter '(34 4)) (setq beat0 1/8 beat1 1/4 beat2 2/4 beat3 4/4)) ;no 24, as 4/4

         (t (warn " get-beat-level-fractions, meter not defined: ~A ,using (4 4) instead" meter))
         )
        (list beat0 beat1 beat2 beat3)
        ))

;mark the duration in ms of each beat level
;only at meter mark
(defun mark-beat-fraction-dr ()
  (let (beat-value factor fractions)
    (each-note
       (when (this 'meter)
           (setq beat-value (cadr (this 'meter))) )
       (when (this 'mm)
           (setq factor (/ (* beat-value 60000.0) (this 'mm))) )
       (when (this 'meter)
           (setq fractions (get-beat-fractions (this 'meter)))
           ;(print-ll " fractions " fractions)
           (set-this :beat0dr (* factor (first fractions)))
           (set-this :beat1dr (* factor (second fractions)))
           (set-this :beat2dr (* factor (third fractions)))
           (set-this :beat3dr (* factor (fourth fractions)))
           )
     )))

(defun scale-beat-salience ()
  (let ((m 2) (s 1.65) ; mean and SD for gauss distribution
        beat0dr beat1dr beat2dr beat3dr )
    (each-note
     (when (this :beat0dr) (setq beat0dr (/ (this :beat0dr) 1000.0)))
     (when (this :beat1dr) (setq beat1dr (/ (this :beat1dr) 1000.0)))
     (when (this :beat2dr) (setq beat2dr (/ (this :beat2dr) 1000.0)))
     (when (this :beat3dr) (setq beat3dr (/ (this :beat3dr) 1000.0)))
     
     (when (this :beat0sal) 
       (set-this :beat0sal (* (this :beat0sal) (log-gauss-trans beat0dr m s)))
       ;(print-ll "log-gauss-trans " (log-gauss-trans beat0dr m s))
       )
     (when (this :beat1sal) (set-this :beat1sal (* (this :beat1sal) (log-gauss-trans beat1dr m s))))
     (when (this :beat2sal) (set-this :beat2sal (* (this :beat2sal) (log-gauss-trans beat2dr m s))))
     (when (this :beat3sal) (set-this :beat3sal (* (this :beat3sal) (log-gauss-trans beat3dr m s))))
     )))

(defun log-gauss-trans (x m s)
  (let ((xlog (log x))
        (mlog (log m))
        (slog (log s)) )
    (exp (* -0.5 (expt (/ (- xlog mlog) slog) 2)))
    ))


;-----------------------------------------------------------------------
;------------- melodic accent old model --------------------------------
;-----------------------------------------------------------------------


;main function
;***** replaced by a new model below ****
(defun mark-melodic-accent-old ()
  (rem-all 'accent-c)
  ;mark metrical levels
  (mark-mel-peak-and-valley)
  (mark-mel-salience-dev-from-mean)
  (mark-mel-salience-prev-interval)
  (mark-mel-final-combination)
  )

(defun mark-mel-peak-and-valley ()
  (rem-all :peak)(rem-all :valley)
  (each-note-if
   (not (first?))
   (not (last?))   
   (not (prev 'rest))
   (not (this 'rest))
   (not (next 'rest))
   (> (this-f0) (prev-f0))
   (>= (this-f0) (next-f0))
   (then
    (set-this :peak t) ))
  (each-note-if
   (not (first?))
   (not (last?))   
   (not (prev 'rest))
   (not (this 'rest))
   (not (next 'rest))
   (< (this-f0) (prev-f0))
   (<= (this-f0) (next-f0))
   (then
    (set-this :valley t) )) )

;; mark on all notes except repeats
(defun mark-mel-peak-and-valley ()
  (rem-all :peak)(rem-all :valley)
  (each-note-if
   (not (first?))
   (not (last?))   
   (not (prev 'rest))
   (not (this 'rest))
   ;(not (next 'rest))
   (> (this-f0) (prev-f0))
   ;(>= (this-f0) (next-f0))
   (then
    (set-this :peak t) ))
  (each-note-if
   (not (first?))
   (not (last?))   
   (not (prev 'rest))
   (not (this 'rest))
   ;(not (next 'rest))
   (< (this-f0) (prev-f0))
   ;(<= (this-f0) (next-f0))
   (then
    (set-this :valley t) )) )

(defun mark-mel-salience-dev-from-mean ()
  (rem-all :melsal1)
  (each-track
   ;compute mean f0 for the track
   (let ((mean-f0 0) (n-f0 0))
     (each-note-if
      (this 'f0)
      (incf mean-f0 (this-f0))
      (incf n-f0) )
     (setq mean-f0 (float (/ mean-f0 n-f0)))
     (print-ll "mean f0 of this track = " mean-f0) 
     (each-note-if
      (this :peak)
      (then
       (set-this :melsal1 (float (/ (max 0 (- (this-f0) mean-f0)) 4))) ))
     (each-note-if
      (this :valley)
      (then
       (set-this :melsal1 (float (/ (max 0 (- mean-f0 (this-f0))) 12))) ))
     )))

;new version with running average
(defun mark-mel-salience-dev-from-mean ()
  (rem-all :melsal1)
  (rem-all :f0-run-mean)
  (mark-running-mean-2-bars-or-10-notes)
  (each-note-if
   (this :peak)
   (then
    (set-this :melsal1 (float (/ (max 0 (- (this-f0) (this :f0-run-mean))) 4))) ))
  (each-note-if
   (this :valley)
   (then
    (set-this :melsal1 (float (/ (max 0 (- (this :f0-run-mean) (this-f0))) 12))) ))
  )


(defun mark-mel-salience-prev-interval ()
  (rem-all :melsal2)
  (each-track
   ;compute mean f0 for the track
   (each-note-if
    (this :peak)
    ;(> (abs (- (this-f0) (prev-f0))) 2)
    (then
     (set-this :melsal2 (float (/ (abs (- (this-f0) (prev-f0))) 4))) ))
   (each-note-if
    (this :valley)
   ; (> (abs (- (this-f0) (prev-f0))) 2)
    (then
     (set-this :melsal2 (float (/ (abs (- (this-f0) (prev-f0))) 12))) ))
   ))

#|
(defun mark-mel-final-combination ()
  (each-note-if
   (or (this :peak) (this :valley))
   (then
    (set-this 'accent-c (min 5 (round (* 1.5 (+ (this :melsal1) (this :melsal2))))))
    )))
|#

(defun mark-mel-final-combination ()
  (each-note-if
   (or (this :peak) (this :valley))
   (then
    (let ((sal (min 5 (* 1.5 (* (this :melsal1) (this :melsal2))))))
    (if (> sal 0) (set-this 'accent-c sal))
      ))))


;-----------------------------------------------------------------------
;--------------------------new melodic accent---------------------------
;-----------------------------------------------------------------------

; immanent melodic accent model in Bisesi-Friberg-Parncutt-2019


;;--- new melodic accent from Richard mail 130513------

;;;1.       No need to mark peaks and valleys. Start by treating all notes equal. We call that musical democracy;-)

;;;2.       :melsal1 is merely the distance of each tone from the mean pitch in semitones. Always positive of course, 
;;;         to do this you will take the "absolute value" of the difference. (Maybe we will have to divide this value by 2 
;;;          for pitches below the mean but we can experiment with that later, first I would leave it out and see what happens.)

;;;3.       :melsal2 is the size of the preceding leap in semitones. Again, the absolute value. 
;;;         (again, we might later decide to divide by two for falling intervals. But not yet)

;;;4.       Just multiply :melsal1 and :melsal2 together. The result is the melodic accent. Every tone in the whole melody gets
;;;         a value.

;;;5.       Maybe we should subtract a threshold, and if the result is negative just make it zero. 
;;;         But I would like to see how steps 1-4 work without doing that. Might be an unnecessary complexity.

#|
(defun mark-melodic-accent-2 ()
  (rem-all 'accent-c)
  (rem-all :melsal1)
  (rem-all :melsal2)
  (rem-all :melsal3)
  (each-track
   ;compute mean f0 for the track
   (let ((mean-f0 0) (n-f0 0))
     (each-note-if
      (this 'f0)
      (incf mean-f0 (this-f0))
      (incf n-f0) )
     (setq mean-f0 (float (/ mean-f0 n-f0)))
     (print-ll "mean f0 of this track = " mean-f0) 
     (each-note-if
      (not (first?))
      (not (this 'rest))
      (not (prev 'rest))
      (then
       (set-this :melsal1 (abs (- (this-f0) mean-f0)))
       (set-this :melsal2 (abs (- (this-f0) (prev-f0))))
       (set-this :melsal3 (* (this :melsal1) (this :melsal2)))
       (let ((salience (round (/ (this :melsal3) 15.0))))
         (if (> salience 0) (set-this 'accent-c salience)) )
       )))))


(defun mark-melodic-accent ()
  (rem-all 'accent-c)
  (rem-all :melsal1)
  (rem-all :melsal2)
  (rem-all :melsal3)
  (each-track
   ;compute mean f0 for the track
   (let ((mean-f0 0) (n-f0 0))
     (each-note-if
      (this 'f0)
      (incf mean-f0 (this-f0))
      (incf n-f0) )
     (setq mean-f0 (float (/ mean-f0 n-f0)))
     (print-ll "mean f0 of this track = " mean-f0) 
     (each-note-if
      (not (first?))
      (not (this 'rest))
      (not (prev 'rest))
      (then
       (let ((f0-dist-mean (- (this-f0) mean-f0))
             (f0-int (- (this-f0) (prev-f0))) )
         (if (> f0-dist-mean 0) (set-this :melsal1 f0-dist-mean))      ;above mean
         (if (<= f0-dist-mean 0) (set-this :melsal1 (abs (* f0-dist-mean 0.7)))) ;below mean
         (if (> f0-int 0) (set-this :melsal2 f0-int))                  ;rising interval
         (if (<= f0-int 0) (set-this :melsal2 (abs (* f0-int 0.7)))) ) ;falling interval
         (set-this :melsal3 (* (this :melsal1) (this :melsal2)))
       (let ((salience (round (/ (this :melsal3) 15.0))))
         (if (> salience 0) (set-this 'accent-c salience)) )
       )))))

;new version with running average
(defun mark-melodic-accent ()
  (rem-all 'accent-c)
  (rem-all :melsal1)
  (rem-all :melsal2)
  (rem-all :melsal3)
  (mark-running-mean-2-bars-or-10-notes) 
     (each-note-if
      (not (first?))
      (not (this 'rest))
      (not (prev 'rest))
      (then
       (let ((f0-dist-mean (- (this-f0) (this :f0-run-mean)))
             (f0-int (- (this-f0) (prev-f0))) )
         (if (> f0-dist-mean 0) (set-this :melsal1 f0-dist-mean))      ;above mean
         (if (<= f0-dist-mean 0) (set-this :melsal1 (abs (* f0-dist-mean 0.7)))) ;below mean
         (if (> f0-int 0) (set-this :melsal2 f0-int))                  ;rising interval
         (if (<= f0-int 0) (set-this :melsal2 (abs (* f0-int 0.7)))) ) ;falling interval
         (set-this :melsal3 (* (this :melsal1) (this :melsal2)))
       (let ((salience (round (/ (this :melsal3) 15.0))))
         (if (> salience 0) (set-this 'accent-c salience)) )
       )))

;sqrt in end
(defun mark-melodic-accent ()
  (rem-all 'accent-c)
  (rem-all :melsal1)
  (rem-all :melsal2)
  (rem-all :melsal3)
  (mark-running-mean-2-bars-or-10-notes) 
     (each-note-if
      (not (first?))
      (not (this 'rest))
      (not (prev 'rest))
      (then
       (let ((f0-dist-mean (- (this-f0) (this :f0-run-mean)))
             (f0-int (- (this-f0) (prev-f0))) )
         (if (> f0-dist-mean 0) (set-this :melsal1 f0-dist-mean))      ;above mean
         (if (<= f0-dist-mean 0) (set-this :melsal1 (abs (* f0-dist-mean 0.7)))) ;below mean
         (if (> f0-int 0) (set-this :melsal2 f0-int))                  ;rising interval
         (if (<= f0-int 0) (set-this :melsal2 (abs (* f0-int 0.2)))) ) ;falling interval
         (set-this :melsal3 (* (this :melsal1) (this :melsal2)))
       (let ((salience (/ (sqrt (this :melsal3)) 2.5)))
         (if (> salience 0) (set-this 'accent-c salience)) )
       ))
  (mark-melodic-accent-retain-max-of-three)
  )

  |#

;with removal of stepwise motion
; final version for Bisesi-Friberg-Parncutt-2019
(defun mark-melodic-accent ()
  (rem-all 'accent-c)
  ;(rem-all :melsal1)
  ;(rem-all :melsal2)
  ;(rem-all :melsal3)
  (mark-running-mean-2-bars-or-10-notes) 
     (each-note-if
      (not (first?))
      (not (this 'rest))
      (not (prev 'rest))
      (then
       (let ((f0-dist-mean (- (this-f0) (this :f0-run-mean)))
             (f0-int (- (this-f0) (prev-f0))) )
         (if (> f0-dist-mean 0) (set-this :melsal1 f0-dist-mean))      ;above mean
         (if (<= f0-dist-mean 0) (set-this :melsal1 (abs (* f0-dist-mean 0.7)))) ;below mean
         (if (> f0-int 0) (set-this :melsal2 f0-int))                  ;rising interval
         (if (<= f0-int 0) (set-this :melsal2 (abs (* f0-int 0.2)))) ) ;falling interval
         (set-this :melsal3 (* (this :melsal1) (this :melsal2)))
       (let ((salience (/ (sqrt (this :melsal3)) 2.5)))
         (if (> salience 0) (set-this 'accent-c salience)) )
       ))
  (mark-melodic-accent-remove-stepwise)
  (mark-melodic-accent-retain-max-of-three)
  (rem-all :melsal1)
  (rem-all :melsal2)
  (rem-all :melsal3)
  )

; put the salience to zero if middle of stepwise motion up or down
(defun mark-melodic-accent-remove-stepwise ()
  (each-note-if
   (not (first?))
   (not (last?))
   (this 'accent-c)
   (not (this 'rest))
   (not (prev 'rest))
   (not (next 'rest))
   (then
    (let ((int1 (- (this-f0) (prev-f0)))
          (int2 (- (next-f0) (this-f0))))
    (if (or (and (or (= int1 1) (= int1 2))
                  (or (= int2 1) (= int2 2)) )
            (and (or (= int1 -1) (= int1 -2))
                  (or (= int2 -1) (= int2 -2)) ))
        (set-this 'accent-c 0) )))))


; keeps only the most salient peak in the context of three notes
(defun mark-melodic-accent-retain-max-of-three ()
  (each-note-if
   (not (first?))
   (not (last?))
   (this 'accent-c)
   (then
    (if (or (and (prev 'accent-c) (> (prev 'accent-c) (this 'accent-c)))
            (and (next 'accent-c) (> (next 'accent-c) (this 'accent-c))) )
        (set-this :rem t)
      )))
  (each-note-if
   (this :rem)
   (then
    (rem-this 'accent-c)
    (rem-this :rem)
    )))
                 


;---------- melodic accent utility functions ----------------

;computes a running mean of f0 up to (but not including) the current note
;the starting point is the first note on the bar line one/two/three measures in the past
(defun mark-running-mean-1-bar ()
  (each-track
   (let ((ibar1 0) (ibar2 0)
         (istart 0) )
     (each-note
      (when (this 'bar)
        (setq ibar1 ibar2)
        (setq ibar2 *i*) )
      (if (not (this 'rest)) (set-this :f0-run-mean (compute-mean-f0 ibar1 *i*)))
      ))))

(defun mark-running-mean-2-bars ()
  (each-track 
   (let ((ibar1 0) (ibar2 0) (ibar3 0)
         (istart 0) )
     (each-note
      (when (this 'bar)
        (setq ibar1 ibar2)
        (setq ibar2 ibar3)
        (setq ibar3 *i*) )
      (if (not (this 'rest)) (set-this :f0-run-mean (compute-mean-f0 ibar1 *i*)))
      ))))

;220408 problem if there are no bars indicated as in the jazz data
(defun mark-running-mean-2-bars-or-10-notes ()
  (each-track 
   (let ((ibar1 0) (ibar2 0) (ibar3 0)
         (istart 0) )
     (each-note
      (when (this 'bar)
        (setq ibar1 ibar2)
        (setq ibar2 ibar3)
        (setq ibar3 *i*) )
      (if (< (compute-number-of-notes-excluding-rests ibar1 *i*) 10) ;go further back if few notes
          (setq ibar1 (get-i-n-notes-before-excluding-rests 10)) )
      ;(print-ll " bar = " (this 'bar) " *i* = " *i* " ibar1 = " ibar1)
      (if (not (this 'rest)) (set-this :f0-run-mean (compute-mean-f0 ibar1 *i*)))
      ))))

;220408 new version that is instead starting at the length of two bars before the current note
; will be slightly different but better since the length will be more constant
; needs a meter mark and tempo mark on first note
(defun mark-running-mean-2-bars-length-or-10-notes ()
  (let ((meter 0)(tempo 0)(drlength 0) i2bars)
    (each-note
      (when (this 'meter) (setq meter (this 'meter)))
      (when (this 'mm) (setq tempo (this 'mm)))
      (setq drlength (* 2 (* (car meter) (/ 60000 tempo))))
      (setq i2bars (get-i-dr-length-before drlength))
      (when (< (compute-number-of-notes-excluding-rests i2bars *i*) 10) ;go further back if few notes
          (setq i2bars (get-i-n-notes-before-excluding-rests 10)) )
      ;(if (= *i* 20) (print-ll " drlength " drlength " i2bars = " i2bars))
      (if (not (this 'rest)) (set-this :f0-run-mean-2bar-len (compute-mean-f0 i2bars *i*)))
      )))
;a shorter version
(defun mark-running-mean-1-bar-length-or-5-notes ()
  (let ((meter 0)(tempo 0)(drlength 0) i1bar)
    (each-note
      (when (this 'meter) (setq meter (this 'meter)))
      (when (this 'mm) (setq tempo (this 'mm)))
      (setq drlength (* 1 (* (car meter) (/ 60000 tempo))))
      (setq i1bar (get-i-dr-length-before drlength))
      (when (< (compute-number-of-notes-excluding-rests i1bar *i*) 5) ;go further back if few notes
        (setq i1bar (get-i-n-notes-before-excluding-rests 5))
        ;(print-ll "*i* = " *i* " less than 5 notes in bar, new start: " i1bar)
        )
      ;(if (= *i* 20) (print-ll " drlength " drlength " i1bar = " i1bar))
      (if (not (this 'rest)) (set-this :f0-run-mean-1bar-len (compute-mean-f0 i1bar *i*)))
      )))


(defun mark-running-mean-3-bars ()
  (each-track
   (let ((ibar1 0) (ibar2 0) (ibar3 0) (ibar4 0)
         (istart 0) )
     (each-note
      (when (this 'bar)
        (setq ibar1 ibar2)
        (setq ibar2 ibar3)
        (setq ibar3 ibar4)
        (setq ibar4 *i*) )
      (if (not (this 'rest)) (set-this :f0-run-mean (compute-mean-f0 ibar1 *i*)))
      ))))

(defun compute-mean-f0 (i1 i2)
  ; compute the mean pitch from note i1 to i2 not including i2
  (let ((mean 0) (n 0))
    (loop for i from i1 to (max 0 (1- i2)) do
          (when (not (iget i 'rest))
            (incf n)
            (incf mean (iget-f0 i))
            ))
    (if (plusp n) (float (/ mean n))
      (iget-f0 i2) ) ; first note
    ))

 ; compute the number of notes excluding rests from i1 to i2 not including i2
(defun compute-number-of-notes-excluding-rests (i1 i2)
  (let ((n 0))
    (loop for i from i1 to (max 0 (1- i2)) do
          (when (not (iget i 'rest))
            (incf n) ))
    n
    ))

(defun get-i-n-notes-before-excluding-rests (n)
  (let ((nsum 0) (iresult 0))
    (if (<= *i* 1) 
        (setq iresult 0)
      (loop for i from (1- *i*) downto 0 do
            (when (not (iget i 'rest))
              (incf nsum)
              (setq iresult i) )
            (when (= nsum n)
              (return) )))
    iresult
      ))

;get the note index for the note occuring at drlength before current note
(defun get-i-dr-length-before (drlength)
  (let ((drsum 0) (iresult 0) )
    (if (<= *i* 1) 
        (setq iresult 0)
      (loop for i from (1- *i*) downto 0 do
              (incf drsum (iget i 'dr))
              (setq iresult i)
            (when (>= drsum drlength)
              (return) )))
    iresult
      ))

#|
(defun test ()
  (each-note-if
   (= *i* 12)
   (then
    (print-ll " i " (get-i-n-notes-before-excluding-rests 10))
    )))

(defun test ()
  (each-note-if
   (= *i* 12)
   (then
    (print-ll " i " (get-i-dr-length-before 1000))
    )))
|#



;-----------------------------------------------------------------------
;------------- immanent harmonic accent --------------------------------
;-----------------------------------------------------------------------

; early version that uses the harmonic-charge rule
; harmonic analysis must be added manually to the score
; check where it was used erica?
; NOT used in Bisesi-Friberg-Parncutt-2019


(defun mark-harmonic-accent ()
  (if (or (not (get-first 'q)) (not (get-first 'key))) 
    (Print-ll "mark-harm-accent : no chord or key on first note - skipping rule")
    (progn 
      (rem-all 'accent-h)
      (accent-harm-charge)
      )))

(defun accent-harm-charge ()
  (let ((key)(sal)(quant 1))
    (each-note 
     (if (this 'key) (setq key (this 'key)))
     (when (and (this 'q) (not (this 'rest)))   ; not on rests
       (setq sal (* quant 1.5 (sqrt (harmonic-charge-fn (this 'q) key nil))))
       (if (> sal 0) (set-this 'accent-h sal))
       ))))
  
;-----------------------------------------------------------------------
;-------------- batch processing for testing ---------------------------
;-----------------------------------------------------------------------
#|
(setq *batch-rules*
      '((mark-metrical-accent)
        (METRICAL-ACCENT 1.5 :CURVE :QUADRATIC :amp 0.25)
        ))

(setq *batch-rules*
      '((mark-melodic-accent)
        (melodic-contour-ACCENT 1 :CURVE :QUADRATIC :width 2)
        ))
(setq *batch-rules*
      '((mark-harmonic-accent)
        (harmonic-accent 1 :CURVE :QUADRATIC :width 2)
        ))

(defun run-accent-batch ()
  (let ((dir (ask-user-for-directory)) (comp 1))
    (when dir
      (let ((dir-list (directory dir)))
        ;(pprint dir-list)
        (dolist (fpath dir-list)
          ;(print-ll "fpath = " fpath)
          (when (string-equal (pathname-type fpath) "mus")
            (print-ll "   fpath = " fpath)
            (load-score-fpath fpath)
            (each-track (setf (synth *this-track*) (make-synth "Roland-1010")) ) ; set synth
            (rule-interaction-apply-rules-sync *batch-rules* comp)
            (save-performance-fpath (merge-pathnames (make-pathname :type "per") fpath))
            (save-performance-midifile1-fpath (merge-pathnames (make-pathname :type "mid") fpath))
            ))))))

;-----------------------------------------------------------------------
;-------------- calibration for Disklavier -----------------------------
;-----------------------------------------------------------------------
;;hand-adjusted compensations for differences across the keyboard as estimated by Erica
;;added 170131
;; Possibly dependent on piano and mic position
;; used for the recording of melodies in Friberg-Bisesi-Addessi-Baroni-2019

(defun disklavier-compensation (k)
  (each-note-if
    (this-f0)
    (>= (this-f0) 36)
    (<= (this-f0) 96)
    (then
      (add-this 'sl (* k (nth (- (this-f0) 36) *disklavier-compensation*)))
      )))

;; list starts at F0=36
(setq *disklavier-compensation*
      '(0.5 -0.2 -0.75 -0.75 -0.55 0.65 -0.4 -1.2 -0.75 -1.8 -0.6 0.05 0.67 -1.3 0 -0.2 0.25 1.1 1 -0.25 0.2 -0.2 -0.2 -1.48 3.35 2.5 0.2
            -0.1 -0.7 -0.55 0 0.3 1.95 0.3 -0.15 -0.9 1.4 1.4 1.09 2.3 1.24 2.3 3.4 2.3 2.9 2.55 2.2 2.9 5 3.1 3.2 3.2 6.1 2.9 2.9 3.3
            4.15 3.7 4.65 4.2 4 ))
|#

