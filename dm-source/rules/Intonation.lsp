;;;-*-Mode: LISP; Package: DM -*-
;;
;; **************
;;   INTONATION
;; **************
;;
;; 9202 cl /af
;; 9706 /vc
;;061019/af fixed so that new chord symbols could be read

(in-package :dm)

; ===============================================
;   FUNCTIONS THAT ARE PRESENT IN THIS FILE
; 
; mixed-intonation (quant &key (velocity 0.0047))
; high-sharp (quant)
; melodic-intonation (quant)
;   (melodic-intonation1)
;   (melodic-intonation2 quant)
;   (melodic-intonation3) )
; just-intonation-from-key ()
;    
; harmonic-intonation ()
; harmonic-intonation-env ()
; harmonic-intonation-env-test (&key velocity)
; harmonic-intonation-env-2 ()
; 
; repetition? ()
;    
; this-pure-interval (qnr)
; this-pure-interval-note (qnr n)
; pure-intervals (semit)
; just-intervals (semit)
; 
; this-dr-bind ()
; 
; prev-last-dc ()
; 
; gen-env-list-cos (stime etime sval eval nr)
; gen-env-list-lin (stime etime sval eval nr)
; gen-env-list-lin-delay (stime delay etime sval eval nr)
; gen-env-list-vel (stime sval eval vel)
; gen-env-list-vel-time (stime delay etime sval eval vel)


;----- intonation -----------------------------------------

(defun mixed-intonation (quant &key (velocity 0.0047))
  (when (not (sync-voice-p))
    (melodic-intonation1)
    (melodic-intonation2 quant)
    (rem-all 'df0)
    (make-syncmel)
    (rem-all :oondr)
    (rem-all :mc)
    (harmonic-intonation-env-test :velocity velocity)
    (harmonic-intonation-env-2)
    ))

;new score format
(defun mixed-intonation (quant &key (velocity 0.0047))
    (melodic-intonation1)
    (melodic-intonation2 quant)
    (rem-all 'df0)
    (make-simple-syncmel)
    (rem-all :oondr)
    (rem-all :mc)
    (harmonic-intonation-env-test :velocity velocity)
  (harmonic-intonation-env-2)
  (remove-last-track)
    )

;(defun foo () (load-music-f "debussygm4")(mixed-intonation 1))

;-------------high high -----------------------------------

;; -----------------------
;;   RULE **HIGH-SHARP**
;; -----------------------
;;
;; raising the pitch by 4 cent/octave rel C3 
;; dc = dc + (f0 with zero in C4)*quant*4/12

(defun high-sharp (quant)
   (each-note-if
     (not (this 'rest))
     (then
       (set-this 'dc
         (round
           (+ (or (this 'dc) 0)
              (* (/ 4 12.) quant (- (this-f0) 60))
              ))))))

;---------- melodic intonation ------------------------------------

;melodic intonation
(defun melodic-intonation (quant)
  (melodic-intonation1)
  (melodic-intonation2 quant)
  (melodic-intonation3) )

#|
(defun melodic-intonation-kvint (quant)
  (melodic-intonation1)
  (melodic-intonation2-kvint quant)
  (melodic-intonation3) )
|#

;set the f0 difference to next note
(defun melodic-intonation1 ()
 (each-note-if
   (not (last?))
   (not (this 'rest))
   (not (next 'rest))
   (not (last?))
   (then 
      (set-this 'df0 (- (next-f0) (this-f0))) ;the f0 difference to next tone
   ))
 (each-note-if
   (not (this 'rest))
   (not (this 'dc))
   (then 
      (set-this 'dc 0)))
 )

#|
;;8805
(defun melodic-intonation2 (quant)
 (let ((one)(case))
  (each-note
   (if (this 'q)
       (setq one (tone-to-tonnr (car (this 'q)))) )
   (ifn (this 'rest)
     (then
     (setq case (mod (- (tone-to-tonnr (note-to-tone (this 'n)))
                            one )
                        12 ))  
  ;   (print (this 'n) *i* " case " case)
     (case case
        (0 ())        ;do nothing
        (1 (if (this 'df0)
                 (cond ((= (this 'df0) -1)
                        (add-this 'dc (* quant -7)) )
                       ((plusp (this 'df0))
                        (add-this 'dc (* quant 7)) ))))
        (2 (add-this 'dc (* quant 3)))
        (3 (if (this 'df0)
                 (cond 
                       ((= (this 'df0) 1)
                        (add-this 'dc (* quant 7)) )
                       (t
                        (add-this 'dc (* quant -4)) ))))
        (4 (add-this 'dc (* quant 4)))
        (5 (add-this 'dc (* quant 2)))
        (6 (add-this 'dc (* quant 10)))
        (7 (add-this 'dc (* quant 1)))
        (8 (if (this 'df0)
                 (cond ((minusp (this 'df0))
                        (add-this 'dc (* quant -6)) )
                       ((plusp (this 'df0))
                        (add-this 'dc (* quant 7)) ))))
        (9 (add-this 'dc (* quant 4)))
        (10 (add-this 'dc (* quant -4)))
        (11 (add-this 'dc (* quant 9)))
        ))))))

;Lasse:
;liten sekund, liten ters, liten sext skall h�jas n�r den g�r kromatiskt upp
;ett steg. Alla andra fall ska de vara l�ga.
;tonerna runt polerna grundton och kvint sugs upp av dessa.
;f�r inledningston och tritonus exiterar bara alternativet att g� upp.
;tersen har en viss dragningskraft men mycket mindre. "kvarten sugs upp av tersen"

(defun melodic-intonation2 (quant)
 (let ((one)(case))
  (each-note
   (if (this 'q)
       (setq one (tone-to-tonnr (car (this 'q)))) )
   (ifn (this 'rest)
     (then
     (setq case (mod (- (tone-to-tonnr (note-to-tone (this 'n)))
                            one )
                        12 ))  
  ;   (print (this 'n) *i* " case " case)
     (case case
        (0 ())        ;do nothing
        (1 (if (this 'df0)
                 (cond ((= (this 'df0) -1)
                        (add-this 'dc (* quant -7)) )
                       ((plusp (this 'df0))
                        (add-this 'dc (* quant 7)) ))))
        (2 (add-this 'dc (* quant 3)))
        (3 (if (and (this 'df0)(= (this 'df0) 1))
               (add-this 'dc (* quant 7))
               (add-this 'dc (* quant -4)) ))
        (4 (add-this 'dc (* quant 4)))
        (5 (add-this 'dc (* quant 2)))
        (6 (add-this 'dc (* quant 10)))
        (7 (add-this 'dc (* quant 1)))
        (8 (if (this 'df0)
                 (cond ((minusp (this 'df0))
                        (add-this 'dc (* quant -6)) )
                       ((plusp (this 'df0))
                        (add-this 'dc (* quant 7)) ))))
        (9 (add-this 'dc (* quant 4)))
        (10 (add-this 'dc (* quant -4)))
        (11 (add-this 'dc (* quant 9)))
        ))))))
|#

;ny intonering gruppera halvtoner tv� och tv�
(defun melodic-intonation2 (quant)
 (let ((one)(case)(dc))
  (each-note
   ;(if (this 'q) (setq one (tone-to-tonnr (car (this 'q)))))
   (if (this 'q) (setq one (tone-to-tonnr (chord-to-root (this 'q)))))
   (ifn (this 'rest)
    (then
     (setq case (mod (- (tone-to-tonnr (note-to-tone (this 'n))) one) 12))  
     (case case
        (0 (setq dc 0))        ;do nothing
        (1 (setq dc (* quant -7)))
        (2 (setq dc (* quant 3)))
        (3 (setq dc (* quant -4)))
        (4 (setq dc (* quant 4)))
        (5 (setq dc (* quant 2)))
        (6 (setq dc (* quant 10)))
        (7 (setq dc (* quant 1)))
        (8 (setq dc (* quant -6)))
        (9 (setq dc (* quant 4)))
        (10 (setq dc (* quant -4)))
        (11 (setq dc (* quant 9))) )
     (when                 ;special cases for some neighbor tones
        (and (not (first?))
             (not (last?))
             (not (prev 'rest))
             (not (next 'rest))
             (not (= (- (this-f0)(prev-f0)) 1))
             (= (- (next-f0)(this-f0)) 1) )
        (case case
          (1 (setq dc (* quant 7)))
          (3 (setq dc (* quant 4)))
          (8 (setq dc (* quant 6))) ))
     (add-this 'dc (round dc)) )))))

#|
;above values approximated by almost pure fifth's
(defun melodic-intonation2-kvint (quant)
 (let ((one)(case))
  (each-note
   (if (this 'q)
       (setq one (tone-to-tonnr (car (this 'q)))) )
   (ifn (this 'rest)
     (then
     (setq case (mod (- (tone-to-tonnr (note-to-tone (this 'n)))
                            one )
                        12 ))  
  ;   (print (this 'n) *i* " case " case)
     (case case
        (0 ())        ;do nothing
        (1 (if (this 'df0)
                 (cond ((= (this 'df0) -1)
                        (add-this 'dc (* quant -9)) )
                       ((plusp (this 'df0))
                        (add-this 'dc (* quant 10)) ))))
        (2 (add-this 'dc (* quant 3))) ;***
        (3 (if (this 'df0)
                 (cond 
                       ((= (this 'df0) 1)  ;***nytt
                        (add-this 'dc (* quant 13)) )
                       (t
                        (add-this 'dc (* quant -6)) ))))
        (4 (add-this 'dc (* quant 6))) ;***
        (5 (add-this 'dc (* quant -4))) ;***
        (6 (if (this 'df0)
                 (cond ((minusp (this 'df0))
                        (add-this 'dc (* quant -11)) )
                       ((plusp (this 'df0))
                        (add-this 'dc (* quant 9)) ))))
        (7 (add-this 'dc (* quant 1))) ;***
        (8 (if (this 'df0)
                 (cond ((minusp (this 'df0))
                        (add-this 'dc (* quant -8)) )
                       ((plusp (this 'df0))
                        (add-this 'dc (* quant 11)) ))))
        (9 (add-this 'dc (* quant 4)))
        (10 (if (this 'df0)
                 (cond ((minusp (this 'df0))
                        (add-this 'dc (* quant -5)) )
                       ((plusp (this 'df0))
                        (add-this 'dc (* quant 14)) ))))
        (11 (add-this 'dc (* quant 7)))
        ))))))

(defun test-kvint ()
  (set-all 'dc 0)
  (init-music)
  (high-sharp 1)
  (melodic-intonation-kvint 1)
  (play) )

(defun test-int ()
  (set-all 'dc 0)
  (init-music)
  (high-sharp 1)
  (melodic-intonation 1)
  (play) )
|#

;remove df0 and dc if not changed
(defun melodic-intonation3 ()
 (each-note (rem-this 'df0))
 (let ((last-dc 0))
  (each-note-if
    (not (first?)) 
    (this 'dc)
    (then
      (if (= last-dc (this 'dc))
          (rem-this 'dc)
          (setq last-dc (this 'dc)) )))))

;;________________________ Harmonic intonation ______________________


(defun harmonic-intonation ()
 (let ((qnr))
  (each-note 
     ;(if (this 'q) (setq qnr (tone-to-tonnr (car (this 'q)))) )
     (if (this 'q) (setq qnr (tone-to-tonnr (chord-to-root (this 'q)))) )
   (ifn (this 'rest)
    (set-this 'dc
      (pure-intervals
         (mod (- (tone-to-tonnr (note-to-tone (this 'n))) qnr) 12)
          ))))))

;;***wrong def should be relative key and not first chord***
(defun just-intonation-from-key ()
 (let ((keynr))
  (each-note 
   ;(if (this 'key) (setq keynr (tone-to-tonnr (this 'key))) )
   (if (this 'key) (setq keynr (tone-to-tonnr (chord-to-root (this 'q)))) )
   ;(print keynr)
   (ifn (this 'rest)
    (set-this 'dc
      (just-intervals
         (mod (- (tone-to-tonnr (note-to-tone (this 'n))) keynr) 12)
          ))))))      
#|
;only on long notes
;using the old dc value from melodic intonation 
(defun harmonic-intonation-env ()
 (let ((qnr))
  (each-note 
   (if (this 'q)
       (setq qnr (tone-to-tonnr (car (this 'q)))) )
   (if (and (> (this 'dr) 500)
            (not (this 'rest)) )
     (then                           ;then long notes
       (cond ((and (not (first?))
                   (not (this 'q))
                   (or (prev 'tie) (= (prev-f0)(this-f0))) )
         ;     (print "no change in pitch : " *i*)
              (if (listp (prev 'dc))
                  (set-this 'dc (car (reverse (prev 'dc))))
                  (set-this 'dc  (prev 'dc)) ))
             (t (let ((new-dc  (pure-intervals
                         (mod (- (tone-to-tonnr (note-to-tone (this 'n))) qnr) 12) ))
                      (old-dc (this 'dc)) )
                     (incf new-dc (round (* (/ 4 12.) (- (this-f0) 48)))) ;str�ckning
                     (set-this 'dc (append (list 0 old-dc)
                             (gen-env-list 20 490 old-dc new-dc 20) ))))
              ))
  ;   (cond ((and (not (first?))       ;else short notes
  ;               (listp (prev 'dc))
  ;               (not (this 'rest)) )
  ;          (set-this 'dc (car (reverse (prev 'dc)))) )
  ;         ((and (not (first?))
  ;               (not (this 'rest)) )
  ;          (set-this 'dc (prev 'dc)) ))
    ))))

;;constant velocity of change
;;if bind continue as one note if new chord new target
;;long notes with changing harmony should be notated as several connected ones
;;in order to get the targets of the envelopes right
(defun harmonic-intonation-env ()
 (let ((qnr)(vel 0.0047)) ;vel = change velocity in cent/ms(5,5;4,7 cent/s)
  (each-note 
   (if (this 'q) (setq qnr (tone-to-tonnr (car (this 'q)))));update chordnr
   (if  (or (and (> (this-dr-bind) 400)             ;initial trig conditions
                 (not (this 'rest)) )
            (and (not (first?))                     ;also for continuation of bind
                 (prev 'tie)
                 (listp (prev 'dc)) ))
     (then                                          ;then long notes
       (cond ((and (not (first?))                   ;no envelope on these (tonupprep.)
                   (not (this 'q))
                   (not (prev 'rest))
                   (not (prev 'tie))
                   (= (prev-f0)(this-f0)) )
         ;     (print "no change in pitch : " *i*)
              (if (listp (prev 'dc))
                  (set-this 'dc (car (reverse (prev 'dc))))
                  (set-this 'dc  (prev 'dc)) ))
             (t (let* ((new-dc  (pure-intervals      ;envelopes
                         (mod (- (tone-to-tonnr (note-to-tone (this 'n))) qnr) 12) ))
                       (old-dc (this 'dc))
                       (h (round (* (this 'dr) vel))) )
                   ;(incf new-dc (round (* (/ 4 12.) (- (this-f0) 48)))) ;str�ckning
                   (if (and (not (first?))                ;continue for bind
                            (prev 'tie)
                            (listp (prev 'dc)) )
                       (setq old-dc (car (reverse (prev 'dc)))) ) 
                   (let* ((ddc (- new-dc old-dc))
                          (up? (plusp ddc)) )
                    ;(print "ddc " ddc " h " h " " (this 'n))
                    (if (> (abs ddc) 0)
                      (if (> h (abs ddc)) 
                       (set-this 'dc (append (list 0 old-dc)
                                 (gen-env-list
                                     20 (round (/ (abs ddc) vel)) old-dc new-dc 20) ))
                       ;(print "g� till slutet")
                       (set-this 'dc (append (list 0 old-dc)
                                 (gen-env-list
                                     20 (round (- (this 'dr) 20))
                                     old-dc (if up? (+ old-dc h) (- old-dc h)) 20 )))
              )))))))))))

;test
(defun harmonic-intonation-env ()
 (let ((qnr)(vel 0.003)) ;vel = change velocity in cent/ms(5,5;4,7 cent/s)
  (each-note 
   (if (this 'q) (setq qnr (tone-to-tonnr (car (this 'q)))))  ;update chordnr
   (cond
    ((and (or (first?) (not (repetition?)))  ;not prev bind or rep.
          (not (this 'rest))
          (> (this-dr-bind) 400) )
     (let ((start-dc  (this 'dc))
           (target-dc (this-pure-interval qnr)) )
          (set-this 'dc (gen-env-list-vel start-dc target-dc vel)) ))
    ((and (not (first?))                    ;prev bind or repetition
          (not (this 'rest))
          (repetition?) 
          (> (this-dr-bind) 400) )    ;****beh�vs ej h�r egentligen
     (let ((start-dc  (this 'dc))
           (target-dc (this-pure-interval qnr)) )
          (set-this 'dc (gen-env-list-vel start-dc target-dc vel)) ))
           ))))

;;constant velocity of change
;;if bind continue as one note if new chord new target
;;long notes with changing harmony should be notated as several connected ones
;;in order to get the targets of the envelopes right
(defun harmonic-intonation-env ()
 (let ((qnr)(vel 0.0015)) ;vel = change velocity in cent/ms(5,5;4,7 cent/s)
  (each-note 
   (if (this 'q) (setq qnr (tone-to-tonnr (car (this 'q)))))  ;update chordnr
   (cond
    ((and (or (first?) (not (repetition?)))  ;not prev bind or rep.
          (not (this 'rest))
          (> (this-dr-bind) 400) )
     (let ((start-dc  (this 'dc))
           (target-dc (this-pure-interval qnr)) )
          (set-this 'dc (gen-env-list-vel start-dc target-dc vel)) ))
    ((and (not (first?))                    ;prev bind or repetition
          (not (this 'rest))
          (repetition?) 
          (> (this-dr-bind) 400) )    ;****beh�vs ej h�r egentligen
     (let ((start-dc  (prev-last-dc))
           (target-dc (this-pure-interval qnr)) )
          (set-this 'dc (gen-env-list-vel start-dc target-dc vel)) ))
           ))))
|#

;kunde ha tv� olika hastigheter: n�r alla st�mmor �r tillsammans
;en kortare hastiget �n vid polyfoni
;alla bunda noter b�rjar om
;tonh�jdsenveloppen b�rjar vid stime
(defun harmonic-intonation-env ()
 (let ((qnr)
       (vel 0.0047)   ;vel = change velocity in cent/ms(5,5;4,7 cent/s)
       (stime 120) ) ; start time
  (each-note 
   ;(if (this 'q) (setq qnr (tone-to-tonnr (car (this 'q)))))  ;update chordnr
   (if (this 'q) (setq qnr (tone-to-tonnr (chord-to-root (this 'q)))))  ;update chordnr
   (cond
    ((and (not (this 'rest))
          (> (this 'dr) 400) )
     (let ((start-dc  (this 'dc))
           (target-dc (this-pure-interval qnr)) )
          (set-this 'dc (gen-env-list-vel stime start-dc target-dc vel)) ))
           ))))

;(defun foo () (load-music-f "polyphonic:debussygm4")(mixed-intonation 1))


;Uses the syncvoice
;Current syncvoice tone < 400 ms ==> only melodic tuning
;Whenever a new tone in syncvoice the intonation starts over with
;the melodic intonation.
(defun harmonic-intonation-env-test (&key velocity)
 (let ((qnr)(curn)(new?)(start-dc)(target-dc)(curdr)
       (syncv (car *sync-voice*))
       (vel (or velocity 0.0047))   ;vel = change velocity in cent/ms(5,5;4,7 cent/s)
       (stime 120)    ; start time
       (dr-limit 400) )
 (mapc #'(lambda (curv)                                 ;pairwise with sync voice
    (let ((*vlist* (list curv syncv)))
      ;(print *vlist*)
      (p-each-note
        ;(print (v-this-all curv 'rest))
        (if (and (assq curv *cur-notes*) (v-this curv 'q))
            (setq qnr (tone-to-tonnr (car (v-this curv 'q)))) ) ;update chordnr *FIXA F�R NYA ACCORD BETECKNING***
        (when (not (v-this-all curv 'rest))                     ; not if rest in curv
          ;(print *cur-notes*)
          (cond ((assq curv *cur-notes*)                  ;new note ?
                 (setq curn (assq curv *cur-notes*))
                 (setq start-dc (cdr (v-iget curn 'dc)))
                 (setq target-dc (this-pure-interval-note qnr (v-this curv 'n)))
                 (setq new? t)
                 (setq curdr (v-this syncv 'dr))
                 (if (> curdr dr-limit)               ;update dc if syncnote > 400ms
                   (v-set-this curv 'dc
                     (gen-env-list-vel-time
                        0 stime curdr start-dc target-dc vel ))
                   (v-set-this curv 'dc (list 0 start-dc)) )) ;else only startval
                 (t                                       ;update old note
                  (if (> (v-this syncv 'dr) dr-limit) ;update dc if syncnote > 400ms 
                     (v-iset curn 'dc
                       (append (cdr (v-iget curn 'dc))
                         (gen-env-list-vel-time
                            curdr stime (+ curdr (v-this syncv 'dr))
                            start-dc target-dc vel ))))
                  (setq curdr (+ curdr (v-this syncv 'dr))) )
                  )))))
   *vlist* )))

;;try to translate to new system
(defun harmonic-intonation-env-test (&key velocity)
  (let ((qnr)(curn)(new?)(start-dc)(target-dc)(curdr)(curv)
        (syncv (car (last (track-list *active-score*))))
        (all-tracks (track-list *active-score*))
        (vel (or velocity 0.0047))   ;vel = change velocity in cent/ms(5,5;4,7 cent/s)
        (stime 120)    ; start time
        (dr-limit 400) )
    (loop for i from 0 to (- (length all-tracks) 2) do          ;pairwise with sync voice
          (setq curv (nth i all-tracks))
          (setf (track-list *active-score*) (list curv syncv))
          (print (track-list *active-score*))
          (p-each-note
           ;(print (v-this-all curv 'rest))
           (if (and (assoc curv *cur-notes*) (v-this curv 'q))
               (setq qnr (tone-to-tonnr (car (v-this curv 'q)))) ) ;update chordnr *FIXA F�R NYA ACCORD BETECKNING***
           (when (not (v-this-all curv 'rest))                     ; not if rest in curv
             ;(print *cur-notes*)
             (cond ((assoc curv *cur-notes*)                  ;new note ?
                    (setq curn (assoc curv *cur-notes*))
                    (setq start-dc (cdr (v-iget curn 'dc)))
                    (setq target-dc (this-pure-interval-note qnr (v-this curv 'n)))
                    (setq new? t)
                    (setq curdr (v-this syncv 'dr))
                    (if (> curdr dr-limit)               ;update dc if syncnote > 400ms
                        (v-set-this curv 'dc
                                    (gen-env-list-vel-time
                                     0 stime curdr start-dc target-dc vel ))
                      (v-set-this curv 'dc (list 0 start-dc)) )) ;else only startval
                   (t                                       ;update old note
                    (if (> (v-this syncv 'dr) dr-limit) ;update dc if syncnote > 400ms 
                        (v-iset curn 'dc
                                (append (cdr (v-iget curn 'dc))
                                        (gen-env-list-vel-time
                                         curdr stime (+ curdr (v-this syncv 'dr))
                                         start-dc target-dc vel ))))
                    (setq curdr (+ curdr (v-this syncv 'dr))) )
                   ))))
    (setf (track-list *active-score*) all-tracks)
    (each-note-if  ;convert to new break-point functions
     (this 'dc)
     (listp (this 'dc))
     (then
      (set-this 'dc (this-note-make-time-shape 
                     :interpolation :staircase
                     :y-max 200
                     :y-min -200
                     :break-point-list (this 'dc)))
      ))

    ))


(defun harmonic-intonation-env-2 ()
  (each-note-if
    (this 'dc)
    (listp (this 'dc))
    (= 2 (length (this 'dc)))
    (= 0 (car (this 'dc)))
    (then
      (set-this 'dc (cadr (this 'dc)))
      )))

;(make-syncmel)
;(defun foo () (harmonic-intonation-env-test))


(defun repetition? ()
 (if (and (not (prev 'rest))(not (this 'rest)))
  (if (= (this-f0) (prev-f0)) t nil) ))

;returns the target values according to the interval table below
(defun this-pure-interval (qnr)
  (pure-intervals (mod (- (tone-to-tonnr (note-to-tone (this 'n))) qnr) 12)) )

(defun this-pure-interval-note (qnr n)
  (pure-intervals (mod (- (tone-to-tonnr (note-to-tone n)) qnr) 12)) )

;returns the cent deviation for given semitones over the bass note  
(defun pure-intervals (semit)
 (case semit
   (0 0)(1 5)(2 4)(3 16)(4 -14)(5 -2)
   (6 -10)(7 2)(8 14)(9 -16)(10 18)(11 -12)
 ))

;;values for C,C#,D,Eb,E,F,F#,G,Ab,A,Bb,B from Rossing
(defun just-intervals (semit)
 (case semit
   (0 0)(1 -29)(2 4)(3 16)(4 -32)(5 -2)
   (6 -10)(7 2)(8 14)(9 -16)(10 18)(11 -12)
 ))

;(defun foo ()(each-note (print (this-pure-interval (tone-to-tonnr (car (this 'q)))))))

;(defun foo ()(this-pure-interval))
;----- some special access functions -----

;get the duration including bind
(defun this-dr-bind ()
   (let ((dr (this 'dr))
         (i *i*) )
      (while (iget i 'tie)
         (incf i)
         (setq dr (+ dr (iget i 'dr))) )
      dr ))

;(defun foo () (each-note (print (this 'n)(this 'dr) " " (this 'tie) " " (this-dr-bind))))

;returns the last 'dc prop if list the last element of the list 
(defun prev-last-dc ()
  (let ((prev-dc (prev 'dc)))
    (cond 
     ((null prev-dc) nil)
     ((listp prev-dc) (car (reverse prev-dc)))
     ((numberp prev-dc) prev-dc)
     (t (error 'prev-last-dc "wrong type" prev-dc)) )))

;(defun foo (i) (each-note-if (= i *i*)(print (prev-last-dc))))     


;;----- interpolation fkns -------------------------------


;cosine interpolation
; not generating not changed points
(defun gen-env-list-cos (stime etime sval eval nr)
  (let ((l (list 0 sval)) 
        ;(ival (/ (- eval sval) (float nr)))
        (itime (/ (- etime stime) (float nr)))
        (last-val sval) )
   (loop for i from 0 to nr do
     (let* ((time (round (+ stime (* itime i))))
            (val 
             (round
               (+ (/ (+ sval eval) 2.)
                  (* (/ (- sval eval) 2.)
                     (cos (- (/ (* time pi) etime)
                             (/ (* stime pi) etime) )))))))
        (ifn (= val last-val)
         (then
          (newr l time) 
          (newr l val)
          (setq last-val val) ))))
   l ))

;linear interpolation
(defun gen-env-list-lin (stime etime sval eval nr)
  (let ((l (list 0 sval)) 
        (ival (/ (- eval sval) (float nr)))
        (itime (/ (- etime stime) (float nr)))
        (last-val sval) )
   (loop for i from 0 to nr do
     (let* ((time (round (+ stime (* itime i))))
            (val (round (+ sval (* ival i)))) )
        (ifn (= val last-val)
         (then
          (newr l time) 
          (newr l val)
          (setq last-val val) ))))
   l ))

(defun gen-env-list-lin-delay (stime delay etime sval eval nr)
  (let ((l (list (round stime) (round sval))) 
        (ival (/ (- eval sval) (float nr)))
        (itime (/ (- etime (+ stime delay)) (float nr)))
        (last-val sval) )
   (loop for i from 0 to nr do
     (let* ((time (round (+ (+ stime delay) (* itime i))))
            (val (round (+ sval (* ival i)))) )
        (ifn (= val last-val)
         (then
          (newr l time) 
          (newr l val)
          (setq last-val val) ))))
   l ))

;centvel = cent/ms
(defun gen-env-list-vel (stime sval eval vel)
  (let* ((h (round (* (this 'dr) vel)))
         (ddc (- eval sval))
         (up? (plusp ddc))
         (maxnr 20) )        ;max nr of values
    (cond
      ((zerop ddc)
        sval )
      ((> h (abs ddc)) 
       (gen-env-list-lin stime (round (/ (abs ddc) vel)) sval eval maxnr) )
      (t
       (gen-env-list-lin stime (round (- (this 'dr) 20))
                     sval (if up? (+ sval h) (- sval h)) maxnr ))
      )))

(defun gen-env-list-vel-time (stime delay etime sval eval vel)
  (let* ((h (round (* (- etime (+ stime delay)) vel)))
         (ddc (- eval sval))
         (up? (plusp ddc))
         (maxnr 20) )        ;max nr of values
    (setq delay (round delay))
    (cond
      ((zerop ddc)
        (list (round stime) sval) )
      ((> h (abs ddc)) 
       (gen-env-list-lin-delay stime delay (round (+ stime (/ (abs ddc) vel)))
                         sval eval maxnr ))
      (t
       (gen-env-list-lin-delay stime delay etime sval
                        (if up? (+ sval h) (- sval h))
                        maxnr ))
      )))

  
;; check: EOF

