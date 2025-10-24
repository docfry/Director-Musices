;;;-*-Mode: LISP; Package: DM -*-
;;
;; Extracting all features starting with Friberg-Bisesi-Addessi-Baroni-2019
;; With modifications according to Jones-Friberg-2023
;; Original file: optimize-mel-accent-model-4.lsp
;; 251006/af Started 
;; 251010 Rearranged everything and made the complete rule, it can be placed anywhere in the rule list but merges ties and rest

(in-package :dm)

;; some useful commands
; (load-score) file dialog for opening score
; (print-music-round)
; (print-music-prop-round 'n 'ac)

;; ---------------------------------------------
;; ------- Main function -----------------------
;; ---------------------------------------------


; Computes all the features, the resulting model and applies it to SL
; with scaling of groups
; for testing: (dynamic-accent-jf2023-mlr 1 :pitch 1 :timing 1 :meter 1 :phrasing 1 :high-loud 1)
(defun dynamic-accent-jf2023-mlr (quant &key (pitch 1)(timing 1)(meter 1) (phrasing 1) (high-loud 1))
  (mark-dynamic-accent-jf2023-mlr :pitch pitch :timing timing :phrasing phrasing :meter meter :high-loud high-loud)
  (apply-sl-accent-mlr quant) )

;apply the computed accent value to SL
(defun apply-sl-accent-mlr (quant)
  (each-note-if (this :sl-accent-mlr) (then (add-this 'sl (* quant (this :sl-accent-mlr))))) )

;; ---------------------------------------------------------------------
;; --- Apply the MLR dynamics model with all the calculation in lisp ---
;; ---------------------------------------------------------------------

; Computes the dynamics for each note from the trained MLR model 
; 240702 started. Model from Jones-Friberg-2023 using the optimized set of 34 features
; 251006 works with almost the same numbers as in matlab, first 2 decimals correct, possibly round-off error
; new version using feature function
; will preserve the perf
; with scaling of feature groups according to the four main categories in the paper. However, high-loud is separated from the pitch group
(defun mark-dynamic-accent-jf2023-mlr (&key (pitch 1)(timing 1)(meter 1) (phrasing 1) (high-loud 1))
 ;compute all features
  (mark-48-accent-features)
 ; mark the model result
  (each-note-if
    (this :ni)
    (then
      ; The regression equation using 34 features
      (set-this :sl-accent-mlr
                (+
                 0.0242
                 ;pitch contour
                 (* (this :f0-pos-dist-mean) pitch 0.0643)
                 (* (this :f0-neg-dist-mean) pitch -0.0383)
                 (* (this :f0-pos-dist-mean-1bar) pitch  0.0335)   
                 (* (this :f0-bef-pos-leap-p) pitch  0.1297) 
                 (* (this :f0-aft-pos-leap-log) pitch  0.0352)  
                 (* (this :f0-aft-neg-leap-log) pitch -0.0432)  
                 (* (this :f0-aft-leap2-log) pitch  0.0760)
                 (* (this :f0-bef-leap2-log) pitch  0.0814)
                 (* (this :f0-pos-peak-log) pitch -0.0939)  
                 (* (this :f0-neg-peak-log) pitch -0.0862)
                 (* (this :f0-pos-peak3-p) pitch  0.2716)
                 (* (this :f0-first-arp-down-p) pitch  0.1041)
                 ;tempo
                 (* (this :dr-ndr) timing -0.0002)
                 (* (this :dr-very-short-note) timing  0.2841)  
                 ;timing patterns
                 (* (this :dr-short-before-p) timing  0.2804)
                 (* (this :dr-short2-before-p) timing  0.1584)  
                 (* (this :dr-short-after-p) timing  0.2004)
                 (* (this :dr-first-short-p) timing -0.3142)
                 (* (this :dr-short-between-long-p) timing  0.2094)  
                 (* (this :dr-long-after-p) timing -0.1486)
                 ;simple phrasing
                 (* (this :ph-rest-before-or-first) phrasing -0.2653)
                 (* (this :ph-rest-after-or-last) phrasing -0.4807)
                 ;meter
                 (* (if (this :beat0) 1 0) meter 0.0718)  
                 (* (if (this :beat1) 1 0) meter  0.1016)
                 (* (if (this :beat2) 1 0) meter  0.0641)
                 ;automatic rules
                 (* (this :ru-chromch-dsl) pitch -0.0121) 
                 (* (this :ru-hiloud-dsl) high-loud  0.3635)
                 (* (this :ru-durcont-dsl)  timing -0.3092) 
                 (* (this :ru-punct-dro) phrasing -0.0001)  
                 (* (this :ph-punct-first) phrasing -0.4088)
                 ;phrasing rules
                 (* (this :ru-ph4-dsl) phrasing  0.1065)
                 (* (this :ru-ph5-dsl) phrasing  0.1172)
                 (* (this :ru-ph6-dsl) phrasing  0.1476)
                 (* (this :ru-ph6-start) phrasing -0.3946)
                 ))))
  (remove-48-accent-features) )


#| coefficient data from matlab with full precision
0.0242015250010596
0.0642582990395633
-0.0382715300313884
0.0334871973197699
0.129683798888404
0.0352240261102310
-0.0431637902450718
0.0759672583220760
0.0813737568868533
-0.0938656242822384
-0.0861655844065779
0.271563739668946
0.104117117941398
-0.000179469587703835
0.284142537416628
0.280386504874478

0.158445245838616
0.200382800501915
-0.314246622846857
0.209421849706435
-0.148586791761285
-0.265278873049977

-0.480659740870167
0.0718180955464983
0.101653414393652
0.0640646310783855
-0.0120655311538981

0.363472322721374
-0.309247856515593
-0.000135519966867202
-0.408797003294160
0.106508874812647
0.117165982871436
0.147648841552349
-0.394563750938199
|#

#| coefficient data from matlab
constant term = 0.042
--- MLR no CV --- R_mean, all participants, red red features (34)) ---
 ---------- Multiple Regression  ---------- 
Dependent variable: mean_rating
Number of cases =  1701
,                          b,      beta,   sr,     sr2,    p-value,
  f0_pos_dist_mean_2bar,   0.064,  0.140,  0.053,  0.003,  0.001***
  f0_neg_dist_mean_2bar,  -0.038, -0.085,  0.054,  0.003,  0.001***
  f0_pos_dist_mean_1bar,   0.033,  0.071,  0.027,  0.001,  0.074   
      f0_bef_pos_leap_p,   0.130,  0.037,  0.031,  0.001,  0.045*  
    f0_aft_pos_leap_log,   0.035,  0.022,  0.013,  0.000,  0.394   
    f0_aft_neg_leap_log,  -0.043, -0.027,  0.017,  0.000,  0.274   
       f0_aft_leap2_log,   0.076,  0.038,  0.030,  0.001,  0.050*  
       f0_bef_leap2_log,   0.081,  0.040,  0.036,  0.001,  0.019*  
        f0_pos_peak_log,  -0.094, -0.066,  0.035,  0.001,  0.023*  
        f0_neg_peak_log,  -0.086, -0.061,  0.037,  0.001,  0.016*  
         f0_pos_peak3_p,   0.272,  0.063,  0.046,  0.002,  0.003** 
    f0_first_arp_down_p,   0.104,  0.012,  0.011,  0.000,  0.455   
                 dr_ndr,  -0.000, -0.060,  0.043,  0.002,  0.005** 
     dr_very_short_note,   0.284,  0.018,  0.016,  0.000,  0.288   
      dr_short_before_p,   0.280,  0.086,  0.049,  0.002,  0.001** 
     dr_short2_before_p,   0.158,  0.038,  0.027,  0.001,  0.082   
       dr_short_after_p,   0.200,  0.057,  0.027,  0.001,  0.074   
       dr_first_short_p,  -0.314, -0.066,  0.050,  0.003,  0.001** 
dr_short_between_long_p,   0.209,  0.039,  0.030,  0.001,  0.051   
        dr_long_after_p,  -0.149, -0.046,  0.039,  0.002,  0.012*  
ph_rest_before_or_first,  -0.265, -0.051,  0.034,  0.001,  0.026*  
  ph_rest_after_or_last,  -0.481, -0.093,  0.068,  0.005,  0.000***
                  beat0,   0.072,  0.024,  0.019,  0.000,  0.206   
                  beat1,   0.102,  0.038,  0.026,  0.001,  0.088   
                  beat2,   0.064,  0.020,  0.015,  0.000,  0.329   
         ru_chromch_dsl,  -0.012, -0.013,  0.012,  0.000,  0.423   
          ru_hiloud_dsl,   0.363,  0.337,  0.183,  0.034,  0.000***
         ru_durcont_dsl,  -0.309, -0.055,  0.040,  0.002,  0.010*  
           ru_punct_dro,  -0.000, -0.002,  0.001,  0.000,  0.943   
         ru_punct_first,  -0.409, -0.135,  0.091,  0.008,  0.000***
             ru_ph4_dsl,   0.107,  0.070,  0.064,  0.004,  0.000***
             ru_ph5_dsl,   0.117,  0.108,  0.090,  0.008,  0.000***
             ru_ph6_dsl,   0.148,  0.161,  0.121,  0.015,  0.000***
           ru_ph6_start,  -0.395, -0.097,  0.072,  0.005,  0.000***
R2     = 0.608
R2 adj = 0.600
|#


;; --------------------------------------------
;; ---- feature calculations ------------------
;; --------------------------------------------


;; Will mark the complete set of 48 features as described in Jones-Friberg 2023
;; Would be nice to remove merge-all-ties-and-rests but it seems difficult, see optimize-mel-accent-model-4.lsp
(defun mark-48-accent-features ()
  (print-ll "Warning: mark-48-accent-features may change the score structure and will add 48 features, Thus do not save the score afterwards")
  (print-ll "Warning: It also write phrase marks on level 7, thus level 7 can not be used for phrasing")
  ; initial markings
  (merge-all-ties-and-rests) ;can be destructive!, defined in initconvert.lsp, moved earlier, mark-all-metrical-features works?
  (mark-performance-param) ;save perf data
  (reset-music)
  (mark-note-index)
  ;(mark-onset-offset-time) ;abs in s, :onset, :offset, just needed for export
  ;(mark-nom-onset-offset-time) ;abs in s, :nonset, :noffset
  ; features
  (mark-all-metrical-features)
  (mark-all-mel-features)
  (mark-arpeggi-features)
  (mark-rhythm-features)
  (mark-simple-phrasing)
  (mark-very-short-notes)
  (mark-melodic-accent)
  (mark-performance-rules)
  (mark-punctuation-accent)
  ;restore perf data
  (restore-performance-param)
  )

;remove all note parameters that are added in the feature calculation
(defun remove-48-accent-features ()
  ;parameters
  (rem-all :ni) (rem-all :onset) (rem-all :offset) (rem-all :nonset) (rem-all :noffset) 
  (rem-all  :perf-dr) (rem-all :perf-ndr) (rem-all :perf-dro) (rem-all :perf-sl)
  ;not used but calculated
  (rem-all 'ACCENT-C) (rem-all :F0-RUN-MEAN) (rem-all :F0-RUN-MEAN-1BAR-LEN) (rem-all :F0-RUN-MEAN-2BAR-LEN)
  ;pitch features
  (rem-all :f0-pos-dist-mean) (rem-all :f0-neg-dist-mean)
  (rem-all :f0-pos-dist-mean-1bar) (rem-all :f0-neg-dist-mean-1bar)

  (rem-all :f0-bef-pos-leap-p) (rem-all :f0-bef-pos-leap-log)
  (rem-all :f0-bef-neg-leap-p) (rem-all :f0-bef-neg-leap-log)
  (rem-all :f0-aft-pos-leap-p) (rem-all :f0-aft-pos-leap-log)
  (rem-all :f0-aft-neg-leap-p) (rem-all :f0-aft-neg-leap-log) 

  (rem-all :f0-aft-leap2-log) (rem-all :f0-bef-leap2-log)
  (rem-all :f0-pos-peak-p) (rem-all :f0-pos-peak-log) 
  (rem-all :f0-neg-peak-p)  (rem-all :f0-neg-peak-log)
  (rem-all :f0-pos-peak2-p) (rem-all :f0-pos-peak3-p)

  (rem-all :f0-first-arp-up-p) (rem-all :f0-last-arp-up-p) (rem-all :f0-first-arp-down-p) (rem-all :f0-last-arp-down-p)
              ;tempo
  (rem-all :dr-ndr) 
  (rem-all :dr-very-short-note)
              ;timing
  (rem-all :dr-short-before-p) (rem-all :dr-short-before-rel) (rem-all :dr-short-before-log)
  (rem-all :dr-short2-before-p) (rem-all :dr-short3-before-p)
  (rem-all :dr-short-after-p) (rem-all :dr-first-short-p)
  (rem-all :dr-longest-five-p) (rem-all :dr-longest-five-w)
  (rem-all :dr-short-between-long-p) (rem-all :dr-long-after-p)
              ;simple phrasing
  (rem-all :ph-rest-before-or-first) (rem-all :ph-rest-after-or-last)
              
              ;meter
  (rem-all :beat0)(rem-all :beat1) (rem-all :beat2) (rem-all :beat3)
              ;performance rules
  (rem-all :ru-chromch-ddr) (rem-all :ru-chromch-dsl)
  (rem-all :ru-hiloud-dsl)
  (rem-all :ru-durcont-ddr) (rem-all :ru-durcont-dsl)
  (rem-all :ru-doudur-ddr) 
  (rem-all :ru-punct-ddr) (rem-all :ru-punct-dro) (rem-all :ph-punct-first) (rem-all :ph-punct-last)
  (rem-all :ru-ph4-ddr) (rem-all :ru-ph4-dsl) (rem-all :ru-ph4-start) (rem-all :ru-ph4-end)
  (rem-all :ru-ph5-ddr) (rem-all :ru-ph5-dsl) (rem-all :ru-ph5-start) (rem-all :ru-ph5-end)
  (rem-all :ru-ph6-ddr) (rem-all :ru-ph6-dsl) (rem-all :ru-ph6-start) (rem-all :ru-ph6-end)
  )

;save current performance parameters to separate key words
(defun mark-performance-param ()
  (each-note
    (if (this 'dr) (set-this :perf-dr (this 'dr)))
    (if (this 'ndr) (set-this :perf-ndr (this 'ndr))) ;really not needed but..
    (if (this 'dro) (set-this :perf-dro (this 'dro)))
    (if (this 'sl) (set-this :perf-sl (this 'sl)))
    ))

(defun restore-performance-param ()
  (each-note
    (if (this :perf-dr) (set-this 'dr (this :perf-dr)))
    (if (this :perf-ndr) (set-this 'ndr (this :perf-ndr)))
    (if (this :perf-dro) (set-this 'dro (this :perf-dro)))
    (if (this :perf-sl) (set-this 'sl (this :perf-sl)))
    )
  (rem-all :perf-dr)
  (rem-all :perf-ndr)
  (rem-all :perf-dro)
  (rem-all :perf-sl)
  )


; marks the note index excluding rests corresponding to matlab number
; Including ties!
; not sure it is needed
(defun mark-note-index ()
  (let ((n 1))
  (each-note-if
    (not (this 'rest))
    (or (first?) (not (prev 'tie)))
    (then
      ;(print (this 'n))
      (set-this :ni n)
      (incf n) ))))


;; --------- mark metrical features -----------

;using the function mark-beat-levels from the old accent model in accent-analysis.lsp but with added meters
;add syncopation feature
;must be applied before merge-all-ties-and-rests, for right computation of score position
(defun mark-all-metrical-features ()
 (mark-beat-levels)
; synopation should be according to mullenseifen
; (each-note-if
;   (not (this :beat1))
;   (not (this :beat2))
;   (not (this :beat3))
;   (this :beat0)
;   (then
;     (set-this :syncop t) ))
 )


;; --------- mark melodic features -----------

;compute all features relating to pitch
;all notes will be marked (default = 0)

;170816 new version
; finally a new version - tested

;181023 new version for revision of paper
; Added arpeggio separately for first and last note


;220411 added two new running mean functions (defined in accent-analysis)
; might change the old analysis slightly but is a better analysis not based on bar marks
(defun mark-all-mel-features ()
  ;(mark-running-mean-2-bars-or-10-notes) ; sets :f0-run-mean, f0 distance relatively running mean
  (mark-running-mean-2-bars-length-or-10-notes)
  (mark-running-mean-1-bar-length-or-5-notes)
  (each-note-if 
    (this :f0-run-mean-2bar-len)
    (this 'f0)
    (then
      (let ((f0-dist (- (this-f0) (this :f0-run-mean-2bar-len)))) 
        ;(set-this :f0-abs-dist-mean (abs f0-dist))
        (set-this :f0-pos-dist-mean (if (plusp f0-dist) f0-dist 0))
        (set-this :f0-neg-dist-mean (if (minusp f0-dist) (- f0-dist) 0)) )))
  (each-note-if 
    (this :f0-run-mean-1bar-len)
    (this 'f0)
    (then
      (let ((f0-dist (- (this-f0) (this :f0-run-mean-1bar-len))))
        ;(set-this :f0-abs-dist-mean (abs f0-dist))
        (set-this :f0-pos-dist-mean-1bar (if (plusp f0-dist) f0-dist 0))
        (set-this :f0-neg-dist-mean-1bar (if (minusp f0-dist) (- f0-dist) 0)) )))
  (let ((f0-int 0))
    (each-note-if
      (this 'f0)
      (then
        (set-this :f0-aft-pos-leap-p 0)
        (set-this :f0-bef-pos-leap-p 0)
        (set-this :f0-aft-neg-leap-p 0)
        (set-this :f0-bef-neg-leap-p 0)

        (set-this :f0-aft-pos-leap-log 0)
        (set-this :f0-bef-pos-leap-log 0)
        (set-this :f0-aft-neg-leap-log 0)
        (set-this :f0-bef-neg-leap-log 0)
        (set-this :f0-aft-leap2-log 0)
        (set-this :f0-bef-leap2-log 0)

        (set-this :f0-pos-peak-p 0)
        (set-this :f0-pos-peak-log 0)
        (set-this :f0-neg-peak-p 0)
        (set-this :f0-neg-peak-log 0)
        (set-this :f0-pos-peak2-p 0)
        (set-this :f0-pos-peak3-p 0)

            ;at leaps
        (when (and (not (first?)) (prev 'f0) (not (prev 'tie)))
          (setq f0-int (- (this-f0) (prev-f0)))
          (when (> f0-int 2) 
            (set-this :f0-aft-pos-leap-p 1)
            (set-prev :f0-bef-pos-leap-p 1)
            (set-this :f0-aft-pos-leap-log (log (- (abs f0-int) 1) 2))
            (set-prev :f0-bef-pos-leap-log (log (- (abs f0-int) 1) 2)) )
          (when (< f0-int -2)
            (set-this :f0-aft-neg-leap-p 1)
            (set-prev :f0-bef-neg-leap-p 1)
            (set-this :f0-aft-neg-leap-log (log (- (abs f0-int) 1) 2))
            (set-prev :f0-bef-neg-leap-log (log (- (abs f0-int) 1) 2)) ))
        ;at both notes in leap in the context of 4 notes, similar to leap rule in punct but simplified and jumploc in Mullensiefen
        ;weight = log(abs(int2 - (int1)/2 - (int3)/2)) --- skipped this
        ;just added opposite direction in leaps (or rep)
        ;could be improved according to punctuation but a bit complicated....
        (when (and (not (first?)) (not (first+1?)) (not (last?)) (prev 'f0)(prev2 'f0)(next 'f0))
          (let ((f0int1 (- (prev-f0) (prev2-f0)))
                (f0int2 (- (this-f0) (prev-f0)))
                (f0int3 (- (next-f0) (this-f0))) )
            (when (and (> (abs f0int2) 2)(> (abs f0int2) (abs f0int1))(> (abs f0int2) (abs f0int3)) )
              (when (or (and (>= f0int1 0)(< f0int2 0)(>= f0int3 0))  ;up-down-up
                        (and (<= f0int1 0)(> f0int2 0)(<= f0int3 0)) ) ;down-up-down
                (set-this :f0-aft-leap2-log (log (- (abs f0int2) 1) 2))
                (set-prev :f0-bef-leap2-log (log (- (abs f0int2) 1) 2)) ))))

        ;at peaks or troughs
        ;positive peak in three notes, one before one after
        (when (and (not (first?)) (not (last?)) (prev 'f0)(next 'f0)
                   (> (this-f0) (prev-f0)) (>= (this-f0) (next-f0)) )
          (set-this :f0-pos-peak-p 1)
          (set-this :f0-pos-peak-log (log (1+ (abs (- (this-f0) (prev-f0)))) 2)) )
        ;negative peak in three notes
        (when (and (not (first?)) (not (last?)) (prev 'f0)(next 'f0)
                   (< (this-f0) (prev-f0)) (<= (this-f0) (next-f0)) )
          (set-this :f0-neg-peak-p 1)
          (set-this :f0-neg-peak-log (log (1+ (abs (- (this-f0) (prev-f0)))) 2)) )
        ;positive peak in four notes, two before one after
        (when (and (not (first?)) (not (first+1?)) (not (last?)) 
                   (prev 'f0)(prev2 'f0)(next 'f0)
                   (> (this-f0) (prev-f0)) (> (this-f0) (prev2-f0)) (>= (this-f0) (next-f0)) )
          (set-this :f0-pos-peak2-p 1) )
        ;positive peak in five notes, three before one after
        (when (and (not (first?)) (not (first+1?))(not (first+2?))(not (last?)) 
                   (prev 'f0)(prev2 'f0)(prev3 'f0)(next 'f0)
                   (> (this-f0) (prev-f0)) (> (this-f0) (prev2-f0))(> (this-f0) (prev3-f0))(>= (this-f0) (next-f0)) )
          (set-this :f0-pos-peak3-p 1) )
        ))))


;mark beginning/end of only arpeggi going up/down
;new version 181221
(defun mark-arpeggi-features ()
  (each-note-if
    (this 'f0)
    (then 
        (set-this :f0-first-arp-up-p 0)
        (set-this :f0-first-arp-down-p 0)
        (set-this :f0-last-arp-up-p 0)
        (set-this :f0-last-arp-down-p 0)
        ;arpeggi
        ;first note in arpeggio/scale going up and down, 4 note context, 2nd note is the target
        ;anything before except an arpeggio note
        (when (and (not (last?)) (not (last-1?))
                   (this 'f0)(next 'f0)(next2 'f0) )
          (when
              (and (or (first?) (prev 'rest) (not (prev-leap-up))) 
                   (this-leap-up)(next-leap-up) )
            (set-this :f0-first-arp-up-p 1) )
          (when
              (and (or (first?) (prev 'rest) (not (prev-leap-down))) 
                   (this-leap-down)(next-leap-down) )
            (set-this :f0-first-arp-down-p 1) ))

        ;last note in arpeggio/scale going up and down, 4 note context, 3nd note is the target
        (when (and (not (first?)) (not (first+1?))
                   (this 'f0)(prev 'f0)(prev2 'f0) )
          (when
              (and (or (last?) (next 'rest) (not (this-leap-up))) 
                   (prev-leap-up)(prev2-leap-up) )
            (set-this :f0-last-arp-up-p 1) )
          (when
              (and (or (last?) (next 'rest) (not (this-leap-down))) 
                   (prev-leap-down)(prev2-leap-down) )
            (set-this :f0-last-arp-down-p 1) ))
        )))


;; help functions for arpeggi detector
(defun this-leap-up ()
  (let ((df0 (- (next-f0) (this-f0))))
     (if (> df0 2) df0 nil) ))
(defun next-leap-up ()
  (let ((df0 (- (next2-f0) (next-f0))))
     (if (> df0 2) df0 nil) ))
(defun prev-leap-up ()
  (let ((df0 (- (this-f0) (prev-f0))))
     (if (> df0 2) df0 nil) ))
(defun prev2-leap-up ()
  (let ((df0 (- (prev-f0) (prev2-f0))))
     (if (> df0 2) df0 nil) ))

(defun this-leap-down ()
  (let ((df0 (- (next-f0) (this-f0))))
     (if (< df0 -2) df0 nil) ))
(defun next-leap-down ()
  (let ((df0 (- (next2-f0) (next-f0))))
     (if (< df0 -2) df0 nil) ))
(defun prev-leap-down ()
  (let ((df0 (- (this-f0) (prev-f0))))
     (if (< df0 -2) df0 nil) ))
(defun prev2-leap-down ()
  (let ((df0 (- (prev-f0) (prev2-f0))))
     (if (< df0 -2) df0 nil) ))

#|
(defun test-leap-mark ()
  (each-note-if
    (not (last?))
    (not (this 'rest))
    (not (next 'rest))
    (then
      (set-this 'upleap (this-leap-up)) )))
|#

;; --------- mark rhythmic features -----------

; some simple rhythmical context features
; could be improved: include more complex punctuation features, 
;  use dr scaled with parabola according to find main melody

;170816 added :dr-short-before-rel, :dr-short-before-log, :dr-longest-five-p,:dr-longest-five-w, :dr-short-between-long-p
(defun mark-rhythm-features ()
  (each-note-if
    (this 'f0)
    (then 
      (set-this :dr-ndr (this 'ndr))
      (set-this :dr-short-before-p 0)
      (set-this :dr-short-before-rel 0)
      (set-this :dr-short-before-log 0)
      (set-this :dr-short2-before-p 0)
      (set-this :dr-short3-before-p 0)
      (set-this :dr-short-after-p 0)
      (set-this :dr-first-short-p 0)
      (set-this :dr-longest-five-p 0)
      (set-this :dr-longest-five-w 0)
      (set-this :dr-short-between-long-p 0)
      (set-this :dr-long-after-p 0)
      ;fix dr for one tied note
      (when (and (not (last?)) (next 'f0) (this 'tie))  ; could be improved with right computation
        (set-this :dr-ndr (+ (this 'ndr)(next 'ndr))) )
      ; short note before
      (when (and (not (first?)) (prev 'f0) (not (prev 'tie)) (> (this 'ndr) (prev 'ndr)))
        (set-this :dr-short-before-p 1)
        (set-this :dr-short-before-rel (1- (/ (this 'ndr) (prev 'ndr))))  ;proportional version
        (set-this :dr-short-before-log (log (/ (this 'ndr) (prev 'ndr))))  ;log version
        ) 
      ; two equally short notes before
      (when (and (not (first?)) (not (first+1?))(prev 'f0)(prev2 'f0) (not (prev 'tie))
                 (> (this 'ndr)(prev 'ndr)) (> (this 'ndr)(prev2 'ndr)) (= (prev 'ndr) (prev2 'ndr)) )
        (set-this :dr-short2-before-p 1) )
      ; three equally short notes before
      (when (and (not (first?)) (not (first+1?)) (not (first+2?)) (prev 'f0)(prev2 'f0)(prev3 'f0) (not (prev 'tie))
                 (> (this 'ndr)(prev 'ndr)) (> (this 'ndr)(prev2 'ndr)) (> (this 'ndr)(prev3 'ndr))
                 (= (prev 'ndr) (prev2 'ndr)) (= (prev2 'ndr) (prev3 'ndr)) )
        (set-this :dr-short3-before-p 1) )
      ; short note after
      (when (and (not (last?)) (next 'f0) (not (this 'tie)) 
                 (> (this 'ndr) (next 'ndr)) )
        (set-this :dr-short-after-p 1) )
      ; first of at least two short notes (from punctuation)
      (when (and (not (first?)) (prev 'f0) 
                 (not (last?)) (next 'f0) 
                 (not (this 'tie)) (not (prev 'tie)) 
                 (< (this 'ndr) (prev 'ndr))
                 (= (this 'ndr) (next 'ndr)) )
        (set-this :dr-first-short-p 1) )
      ; longest of five notes (from punctuation, the weight according to punct)
      (when (and (not (first?))(not (first+1?)) (prev 'f0) (prev2 'f0)
                 (not (last?))(not (last-1?)) (next 'f0) (next2 'f0)
                 (not (this 'tie)) (not (prev 'tie)) 
                 (>= (this 'ndr) (prev 'ndr))
                 (>= (this 'ndr) (prev2 'ndr))
                 (> (this 'ndr) (next 'ndr))
                 (> (this 'ndr) (next2 'ndr)) )
        (set-this :dr-longest-five-p 1)
        (let ((R (/ (this 'ndr) (+ (prev 'ndr) (this 'ndr) (next 'ndr)))))
          (set-this :dr-longest-five-w (* 1.3 10.5 (- (* 2.7 R) 0.57)) )))
      ; short-between-long, mark first note of three in long-short-long pattern (from punctuation)
      (when (and (not (last?))(not (last-1?)) (next 'f0) (next2 'f0)
                 (not (this 'tie)) (not (next 'tie)) 
                 (> (this 'ndr) (next 'ndr))
                 (< (next 'ndr) (next2 'ndr)) )
        (set-this :dr-short-between-long-p 1) )
      ; long note after (inhibit feature)
      (when (and (not (last?)) (next 'f0) (not (this 'tie)) 
                 (< (this 'ndr) (next 'ndr)) )
        (set-this :dr-long-after-p 1) )
          )))

;simply mark notes before and after rest including first and last
;170816 added
(defun mark-simple-phrasing ()
  (each-note-if
    (this 'f0)
    (then 
      (set-this :ph-rest-before-or-first 0)
      (set-this :ph-rest-after-or-last 0)
      (when (or (first?) (prev 'rest))
        (set-this :ph-rest-before-or-first 1) )
      (when (or (last?) (next 'rest))
        (set-this :ph-rest-after-or-last 1) )
      )))

;181023 new for revision
;adds an accent mark at a punctuated note and at the following note
;220504 changed to 0 kvalue - otherwise it alters the performance!
(defun mark-punctuation-accent ()
  (punctuation 0 :markphlevel7 t)
  (each-note-if
    (this 'f0)
    (then 
      (set-this :ph-punct-first 0)
      (set-this :ph-punct-last 0)
      (when (member 7 (this 'phrase-start))
        (set-this :ph-punct-first 1) )
      (when (member 7 (this 'phrase-end))
        (set-this :ph-punct-last 1) )
      )))

; new version without altering phrase-marks instead reset perf
;FORTSÄTT HÄR EJ KLART
#|
(defun mark-punctuation-accent ()
  (reset-music)
  (punctuation 1 :markphlevel7 nil)
  (each-note-if
    (this 'f0)
    (then 
      (set-this :ph-punct-first 0)
      (set-this :ph-punct-last 0)
      (when (member 7 (this 'phrase-start))
        (set-this :ph-punct-first 1) )
      (when (member 7 (this 'phrase-end))
        (set-this :ph-punct-last 1) )
      )))
|#

; mark very short notes with starting at 1 from zero, a slope from 50 to 150 ms, then 0
(defun mark-very-short-notes ()
  (each-note-if
    (this 'f0)
    (then 
      (set-this :dr-very-short-note 0)
      (if (<= (this 'ndr) 50)
          (set-this :dr-very-short-note 1) )
      (if (and (> (this 'ndr) 50)(< (this 'ndr) 150))
          (set-this :dr-very-short-note (linear-interpolation (this 'ndr) 50 1 150 0) ))
      )))


; 220809 mark a set of performance rules
; according to the rule palette test-rules-as-features-3, thus close to the phrase fitting results
; ddr in percent of ndr
; dsl as the deviation from nsl
; not tested yet -  ADD START BINARY (?)
(defun mark-performance-rules ()
  (reset-music) (chromatic-charge 1.0) (normalize-sl) (normalize-dr)
  (set-all :ru-chromch-ddr 0) (set-all :ru-chromch-dsl 0)
  (each-note-if (this 'dr) (set-this :ru-chromch-ddr (/ (- (this 'dr)(this 'ndr) (this 'ndr)))))
  (each-note-if (this 'sl) (set-this :ru-chromch-dsl (- (this 'sl)(this 'nsl))))

  (reset-music) (HIGH-LOUD 1.0 ) (normalize-sl) 
  (set-all :ru-hiloud-dsl 0)
  (each-note-if (this 'sl) (set-this :ru-hiloud-dsl (- (this 'sl)(this 'nsl))))

  (reset-music) (DURATION-CONTRAST 1.0 :amp 1 :dur 1) (normalize-sl) (normalize-dr)
  (set-all :ru-durcont-ddr 0) (set-all :ru-durcont-dsl 0)
  (each-note-if (this 'dr) (set-this :ru-durcont-ddr (/ (- (this 'dr)(this 'ndr) (this 'ndr)))))
  (each-note-if (this 'sl) (set-this :ru-durcont-dsl (- (this 'sl)(this 'nsl))))

  (reset-music) (DOUBLE-DURATION 1.0 ) (normalize-dr)
  (set-all :ru-doudur-ddr 0) 
  (each-note-if (this 'dr) (set-this :ru-doudur-ddr (/ (- (this 'dr)(this 'ndr) (this 'ndr)))))

  (reset-music) (PUNCTUATION 1.1 :dur 1 :duroff 1 :markphlevel7 nil) (normalize-dr)
  (set-all :ru-punct-ddr 0) (set-all :ru-punct-dro 0)
  (each-note-if (this 'dr) (set-this :ru-punct-ddr (/ (- (this 'dr)(this 'ndr) (this 'ndr)))))
  (each-note-if (this 'dro) (set-this :ru-punct-dro (this 'dro)))

  (reset-music) (Phrase-arch 1.5 :phlevel 4 :turn 0.67 :acc 0.25 :amp 3 :power 3 :decfn :handgest :accfn :handgest) (normalize-sl) (normalize-dr)
  (set-all :ru-ph4-ddr 0) (set-all :ru-ph4-dsl 0) (set-all :ru-ph4-start 0) (set-all :ru-ph4-end 0)
  (each-note-if (this 'dr) (set-this :ru-ph4-ddr (/ (- (this 'dr)(this 'ndr) (this 'ndr)))))
  (each-note-if (this 'sl) (set-this :ru-ph4-dsl (- (this 'sl)(this 'nsl))))
  (each-note-if (this 'phrase-start) (member 4 (this 'phrase-start)) (then (set-this :ru-ph4-start 1)))
  (each-note-if (this 'phrase-end) (member 4 (this 'phrase-end)) (then (set-this :ru-ph4-end 1)))

  (reset-music) (PHRASE-ARCH 1.5 :phlevel 5 :turn 0.5 :acc 1.4 :amp 3 :power 3 :decfn :handgest :accfn :handgest) (normalize-sl) (normalize-dr) ;WHY :acc 1.4 ? MISSTAKE OR ON PURPOSE?
  (set-all :ru-ph5-ddr 0) (set-all :ru-ph5-dsl 0) (set-all :ru-ph5-start 0) (set-all :ru-ph5-end 0)
  (each-note-if (this 'dr) (set-this :ru-ph5-ddr (/ (- (this 'dr)(this 'ndr) (this 'ndr)))))
  (each-note-if (this 'sl) (set-this :ru-ph5-dsl (- (this 'sl)(this 'nsl))))
  (each-note-if (this 'phrase-start) (member 5 (this 'phrase-start)) (then (set-this :ru-ph5-start 1)))
  (each-note-if (this 'phrase-end) (member 5 (this 'phrase-end)) (then (set-this :ru-ph5-end 1)))

  (reset-music) (PHRASE-ARCH 1.5 :phlevel 6 :turn 0.5 :acc 0.1 :amp 3 :power 3 :last 0.5 :decfn :handgest :accfn :handgest) (normalize-sl) (normalize-dr)
  (set-all :ru-ph6-ddr 0) (set-all :ru-ph6-dsl 0) (set-all :ru-ph6-start 0) (set-all :ru-ph6-end 0)
  (each-note-if (this 'dr) (set-this :ru-ph6-ddr (/ (- (this 'dr)(this 'ndr) (this 'ndr)))))
  (each-note-if (this 'sl) (set-this :ru-ph6-dsl (- (this 'sl)(this 'nsl))))
  (each-note-if (this 'phrase-start) (member 6 (this 'phrase-start)) (then (set-this :ru-ph6-start 1)))
  (each-note-if (this 'phrase-end) (member 6 (this 'phrase-end)) (then (set-this :ru-ph6-end 1)))
  (reset-music)
  )


;;---- utilities, testing ---------------

;check if there are any 1/64 written as 1/61 as in 07 and change them, save with the same name (be careful!)
(defun check-for-64th-notes ()
  (let ((dir (show-dialog-for-selecting-directory "select the directory where the mus files are located"))
        (filei 0) (found nil))
    (when dir
        (dolist (fpath (sort (directory dir)  #'string-lessp :key #'namestring))
          (when (string-equal (pathname-type fpath) "mus")
            (setq filei (incf filei))
            (load-score-fpath fpath)
            (setq found nil)
            (each-note-if
              (= 1/61 (second (this 'n)))
              (then
                (setq found t)
                (print (this 'n))
                (set-this 'n (list (car (this 'n)) 1/64))
                ))
            (if found (save-score-fpath fpath)) ;save only if changed
            )))))

;apply mark-beat-levels and print music for all melodies
;just to check that it is proper subdivisions
(defun check-subdivisions-batch ()
  (let ((dir (show-dialog-for-selecting-directory "select the directory where the mus files are located"))
        (filei 0) (found nil))
    (when dir
        (dolist (fpath (sort (directory dir)  #'string-lessp :key #'namestring))
          (when (string-equal (pathname-type fpath) "mus")
            (setq filei (incf filei))
            (load-score-fpath fpath)
            (mark-beat-levels)
            (print-music-round)
            )))))


;;;----------------------------------------------------------------------------------------------------------
;;;--- Exporting all 48 features to a tab-separated text file for Matlab according to Jones-Friberg-2023 ----
;;;----------------------------------------------------------------------------------------------------------


;Saves all mus files in one folder in a text file
(defun print-mel-features-to-file3-2022 ()
  (let ((dir (show-dialog-for-selecting-directory "select the directory where the mus files are located")))
    (when dir (print-mel-features-to-file3-2022-fpath dir)) ))

;220901 New version with performance rules
; Also adapted to just run on one big directory, thus, subject and song nr is extracted from file name
; was used for JonFri2023 files
; 240320 slight adaptation for Ana (old can be restored, see cmments in function)
(defun print-mel-features-to-file3-2022-fpath (dir)
  (let ((filei 0))
      (with-open-file (ofile (merge-pathnames (make-pathname :name (concatenate 'string (car (last (pathname-directory dir))) "-" "accent-features-240705-all.txt")) dir)
                             :direction :output 
                             :if-does-not-exist :create
                             :if-exists :supersede)
        (format ofile "~1&~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A"
          ;file info
          "file_number" #\tab "file" #\tab "song" #\tab "initials" #\tab "subject_nr"
          ;music and performance parameters
          #\tab "note_num" #\tab "F0" #\tab "note" #\tab "notevalue" #\tab "onset" #\tab "offset" #\tab "nonset" #\tab "noffset" #\tab "DR" #\tab "NDR" #\tab "DRO" #\tab "SL"
          ;features, pitch
          ;#\tab "f0_high_loud" 
          #\tab "f0_pos_dist_mean_2bar" #\tab "f0_neg_dist_mean_2bar" 
          #\tab "f0_pos_dist_mean_1bar" #\tab "f0_neg_dist_mean_1bar" 

          #\tab "f0_bef_pos_leap_p" #\tab "f0_bef_pos_leap_log"
          #\tab "f0_bef_neg_leap_p" #\tab "f0_bef_neg_leap_log"
          #\tab "f0_aft_pos_leap_p" #\tab "f0_aft_pos_leap_log" 
          #\tab "f0_aft_neg_leap_p" #\tab "f0_aft_neg_leap_log"

          #\tab "f0_aft_leap2_log" #\tab "f0_bef_leap2_log" 
          #\tab "f0_pos_peak_p" #\tab "f0_pos_peak_log" #\tab "f0_neg_peak_p" #\tab "f0_neg_peak_log"
          #\tab "f0_pos_peak2_p" #\tab "f0_pos_peak3_p" 

          #\tab "f0_first_arp_up_p" #\tab "f0_last_arp_up_p" #\tab "f0_first_arp_down_p" #\tab "f0_last_arp_down_p"
          ;tempo
          #\tab "dr_ndr" 
          #\tab "dr_very_short_note" 
          ;timing
          #\tab "dr_short_before_p" #\tab "dr_short_before_rel" #\tab "dr_short_before_log"
          #\tab "dr_short2_before_p" #\tab "dr_short3_before_p" 
          #\tab "dr_short_after_p" #\tab "dr_first_short_p" 
          #\tab "dr_longest_five_p" #\tab "dr_longest_five_w" 
          #\tab "dr_short_between_long_p" #\tab "dr_long_after_p" 
          ;simple phrasing
          #\tab "ph_rest_before_or_first" #\tab "ph_rest_after_or_last" 
          ;meter
          #\tab "beat0" #\tab "beat1" #\tab "beat2" #\tab "beat3"
          ;performance rules
          #\tab "ru_chromch_ddr" #\tab "ru_chromch_dsl"
          #\tab "ru_hiloud_dsl"
          #\tab "ru_durcont_ddr" #\tab "ru_durcont_dsl"
          #\tab "ru_doudur_ddr" 
          #\tab "ru_punct_ddr" #\tab "ru_punct_dro"  #\tab "ru_punct_first" #\tab "ru_punct_last"
          #\tab "ru_ph4_ddr" #\tab "ru_ph4_dsl" #\tab "ru_ph4_start" #\tab "ru_ph4_end"
          #\tab "ru_ph5_ddr" #\tab "ru_ph5_dsl" #\tab "ru_ph5_start" #\tab "ru_ph5_end"
          #\tab "ru_ph6_ddr" #\tab "ru_ph6_dsl" #\tab "ru_ph6_start" #\tab "ru_ph6_end"

          ;alignment (only for aligned midi performances as in Gabriels exp.)
          #\tab "Align_err_type" #\tab "Align_err_nr"
          )
        (dolist (fpath (sort (directory dir)  #'string-lessp :key #'namestring))
          (when (string-equal (pathname-type fpath) "mus") ;for gabriels files it was (string-equal (pathname-type fpath) "perf")
            (setq filei (incf filei))
            (load-score-fpath fpath)  ; load the score. In gabriels files: (read-active-score-from-file fpath) ;read the performance
             (mark-note-index)
            (mark-performance-param)
            (mark-onset-offset-time) ;abs in s, :onset, :offset
            (mark-nom-onset-offset-time) ;abs in s, :nonset, :noffset

            (reset-music)  ;start from nominal score when computing features!
            (mark-all-metrical-features)
            ;(merge-all-ties-and-rests) ;already done in the alignment
            (mark-all-mel-features)
            (mark-arpeggi-features)
            (mark-rhythm-features)
            (mark-simple-phrasing)
            (mark-very-short-notes)
            (mark-melodic-accent)
            (mark-performance-rules)
            (mark-punctuation-accent) 
            ;(high-loud-varname 1 :varname :sl-high-loud) ;mark high-loud rule with k=1 to another variable
            ;(normalize-sl) ;normalize SL

            (each-note-if
              (this :ni)
              (then
            (format ofile "~1&~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A~A" 
              filei #\tab (file-namestring fpath) 
              ; For gabriels files uncomment this line and comment next
              ;#\tab (extract-songnumber-from-fpath fpath) #\tab (extract-subject-initials-from-fpath fpath) #\tab (convert-initial2nr (extract-subject-initials-from-fpath fpath))
              #\tab 0 #\tab 0 #\tab 0 ;just placeholders, probably needs at least file number
              ;music parameters
              #\tab (this :ni) #\tab (this-f0) #\tab (car (this 'n)) #\tab (cadr (this 'n)) 
              #\tab (this :onset) #\tab (this :offset) #\tab (this :nonset) #\tab (this :noffset) 
              #\tab (this :perf-dr) #\tab (this :perf-ndr) #\tab (if (this :perf-dro) (this :perf-dro) 0) #\tab (this :perf-sl)
              ;features, pitch
              ;#\tab (this :sl-high-loud)
              #\tab (this :f0-pos-dist-mean) #\tab (this :f0-neg-dist-mean)
              #\tab (this :f0-pos-dist-mean-1bar) #\tab (this :f0-neg-dist-mean-1bar)

              #\tab (this :f0-bef-pos-leap-p) #\tab (this :f0-bef-pos-leap-log)
              #\tab (this :f0-bef-neg-leap-p) #\tab (this :f0-bef-neg-leap-log)
              #\tab (this :f0-aft-pos-leap-p) #\tab (this :f0-aft-pos-leap-log)
              #\tab (this :f0-aft-neg-leap-p) #\tab (this :f0-aft-neg-leap-log) 

              #\tab (this :f0-aft-leap2-log) #\tab (this :f0-bef-leap2-log)
              #\tab (this :f0-pos-peak-p) #\tab (this :f0-pos-peak-log) 
              #\tab (this :f0-neg-peak-p)  #\tab (this :f0-neg-peak-log)
              #\tab (this :f0-pos-peak2-p) #\tab (this :f0-pos-peak3-p)

              #\tab (this :f0-first-arp-up-p) #\tab (this :f0-last-arp-up-p) #\tab (this :f0-first-arp-down-p) #\tab (this :f0-last-arp-down-p)
              ;tempo
              #\tab (this :dr-ndr) 
              #\tab (this :dr-very-short-note)
              ;timing
              #\tab (this :dr-short-before-p) #\tab (this :dr-short-before-rel) #\tab (this :dr-short-before-log)
              #\tab (this :dr-short2-before-p) #\tab (this :dr-short3-before-p)
              #\tab (this :dr-short-after-p) #\tab (this :dr-first-short-p)
              #\tab (this :dr-longest-five-p) #\tab (this :dr-longest-five-w)
              #\tab (this :dr-short-between-long-p) #\tab (this :dr-long-after-p)
              ;simple phrasing
              #\tab (this :ph-rest-before-or-first) #\tab (this :ph-rest-after-or-last)
              
              ;meter
              #\tab (if (this :beat0) 1 0) #\tab (if (this :beat1) 1 0) #\tab (if (this :beat2) 1 0) #\tab (if (this :beat3) 1 0)
              ;performance rules
              #\tab (this :ru-chromch-ddr) #\tab (this :ru-chromch-dsl)
              #\tab (this :ru-hiloud-dsl)
              #\tab (this :ru-durcont-ddr) #\tab (this :ru-durcont-dsl)
              #\tab (this :ru-doudur-ddr) 
              #\tab (this :ru-punct-ddr) #\tab (this :ru-punct-dro) #\tab (this :ph-punct-first) #\tab (this :ph-punct-last)
              #\tab (this :ru-ph4-ddr) #\tab (this :ru-ph4-dsl) #\tab (this :ru-ph4-start) #\tab (this :ru-ph4-end)
              #\tab (this :ru-ph5-ddr) #\tab (this :ru-ph5-dsl) #\tab (this :ru-ph5-start) #\tab (this :ru-ph5-end)
              #\tab (this :ru-ph6-ddr) #\tab (this :ru-ph6-dsl) #\tab (this :ru-ph6-start) #\tab (this :ru-ph6-end)
              ;alignment
              #\tab (if (this :align) (this :align) "")  ;write the text
              #\tab (if (this :align)                    ;write the error number
                        (cond
                         ((eq (this :align) :end-of-midi) 1)
                         ((eq (this :align) :missing-rest) 2)
                         ((eq (this :align) :skipping-first-rest-in-midi) 3)
                         ((eq (this :align) :skipping-rest-in-midi) 4)
                         ((eq (this :align) :end-of-midi) 5)
                         ((eq (this :align) :sim-notes-one-match) 6)
                         ((eq (this :align) :sim-notes-no-match) 7)
                         ((eq (this :align) :wrong-pitch) 8)
                         ((eq (this :align) :wrong-pitch-last-or-last-before-rest) 9)
                         ((eq (this :align) :missing-note) 10)
                         ((eq (this :align) :extra-note-before) 11)
                         ((eq (this :align) :no) 12)
                         ((eq (this :align) :sl-zero) 13)
                         (t (error "unknown alignment error: " (this :align))) ) ;doesnt work to print the variable
                      0 ) ; no error - put a zero
              ))))))))



;; -----------------------------------------------------------------------
;; Apply dynamics model on new melodies with the calculation in matlab
;; -----------------------------------------------------------------------

;; STEPS
;; Compute features and save in file: (print-mel-features-to-file3-2022)
;; Matlab: train model and apply it to new data, Save file from matlab. (see section "Use model on other examples" in "perf_accent_analysis_2022.m")
;; /OneDrive-KTH/F1_projekt/2020-Performed-accents-Gabriel/Modelling/perf_accent_analysis_2022.m
;; Add the results to existing scores: (insert-dynamics-and-model-in-mus-score-batch-3)
;; To apply in DM standalone, load the corresponding rule in the command line, see below
 
; this needs to be loaded in DM
(defun dynmod-svr-nocv (quant)
  (each-note-if (this :ni) (then (add-this 'sl (* quant (this :sl-modsvr-nocv))))) )

;--- transfer dynamics model and performance data from matlab to DM -------

;240321 A version for new melodies (not in the dataset for dynamics models) 
; infile format is a CSV file with file_number, note_number, F0,note, notevalue, Dynmodel_svr_nocv
(defun insert-dynamics-and-model-in-mus-score-batch-3 ()
  (let ((dir (show-dialog-for-selecting-directory "select the directory where the mus files are located"))
        (infilepath (show-dialog-for-opening-files-PD "select the input file with data from matlab"))
        (filei 0)
        header-string
        newfilename
        outpath )
    (when (and dir infilepath)
      (with-open-file (infile infilepath :direction :input)
        (setq header-string (read-line infile)) ; read header of input file (not used)

        ; loop the melodies
        (dolist (fpath (sort (directory dir)  #'string-lessp :key #'namestring))
          (when (string-equal (pathname-type fpath) "mus")
            (setq filei (incf filei))
            (load-score-fpath fpath)
            ;(print (namestring fpath))
            (mark-note-index)

            ; loop the notes
            (each-note-if
              (this :ni)
              (then
                (read infile)(read infile)(read infile)(read infile)(read infile) ;read file_number, note_number, F0,note, notevalue
                (set-this :sl-modsvr-nocv (float (/ (round (* (read infile) 100)) 100)))
                ;(set-this :sl-modmlr-nocv (float (/ (round (* (read infile) 100)) 100)))
                ))

            ; save to new DM files
            (setq outpath (merge-pathnames (make-pathname :name (concatenate 'string (pathname-name fpath) "_dynmod")  :type "mus") fpath))
              ;(print outpath) ; just for debugging
            (save-score-fpath outpath)
            ))))))


;; --- utilities ----

;special version for short melodies and phrasing
;needs to be loaded in DM -not working!
(defun normalize-dr-not-last ()
 (each-track
  (let ((ddrtot 0.0)(drtot 0.0))
  ;compute sums (not including last note)
  (each-note-if
    (not (last?))
    (then
      (setq ddrtot (+ (- (this 'dr) (this 'ndr)) ddrtot))
      (setq drtot (+ (this 'dr) drtot)) ))
  ;apply diff to all notes
  (let ((fact (/ (- drtot ddrtot) drtot)))
    (each-note (set-this 'dr (* fact (this 'dr))) )))))


; this can be loaded in DM for accenting the metric structure (didn't work so well though)
; put also (mark-beat-levels) before on the rule palette
(defun add-metric-dyn (quant)
  (each-note-if (this :beat0) (then (add-this 'sl (* 2 quant))))
  (each-note-if (this :beat1) (then (add-this 'sl (* 2 quant))))
  (each-note-if (this :beat2) (then (add-this 'sl (* 2 quant))))
  (each-note-if (this :beat3) (then (add-this 'sl (* 2 quant)))) )


(defun set-pianoteq-all-tracks ()
    (each-track
      (set-track-var 'synth (synt-Pianoteq7-NYStein))) ) ;set the right synth for playing





