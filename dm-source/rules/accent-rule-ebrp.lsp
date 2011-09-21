;; Bisesi/Parncutt accent rules

;;Apply a curve in the sound level and/or the duration in the surrounding of an accent.
;;The music must contain boundary marks in the following way:
;; On the note corresponding to an accent:
;;    left <number of notes involved before the main event (start counting from 0)>
;;    right <number of notes involved after the main event (start counting from 0)>
;;    peak <delta deviation in the sound level or in the duration>
;;    left-fn <function used to model the pattern before the main event>
;;    right-fn <function used to model the pattern after the main event>

;;1002 af/eb : preliminary version
;;101210/af/eb New definitions, fixing more curves, additive but only sound level
;;adapted to graphical interface as defined in accentdialog-win
;;101213/af added accent-dr, integrated them in the rule system with support for polyphony, 
;; multiple rules at the same time
;; a general quantity parameter

(in-package :dm)

;;apply all marked accents in the score
;; default 4 dB for quant or peak = 1
(defun accent-main-sl (quant)
  (each-note-if
   (this 'accent-sl)
   (then
    (let ((left (first (this 'accent-sl)))
          (right (second (this 'accent-sl)))
          (peak (third (this 'accent-sl)))
          (fun-left (fourth (this 'accent-sl)))
          (fun-right (fifth (this 'accent-sl)))
          function-left function-right power-left power-right)
      (multiple-value-setq (function-left power-left) (accents-translate-curv-name-left fun-left))
      (multiple-value-setq  (function-right power-right) (accents-translate-curv-name-right fun-right))
      (apply-accent-sl *i* left right (* quant peak 4.0) function-left function-right power-left power-right)
      ))))

;; Applied the sound level variations on the notes
;; first make 'dsl and then add that to 'sl
(defun apply-accent-sl (note-number ext-left ext-right peak fun-left fun-right power-left power-right)
  (iset-ramp-x2-decimal-last (- note-number ext-left) note-number 0.0 0.0 0.0 peak 'dsl power-left fun-left fun-left)
  (iset-ramp-x2-decimal-last note-number (+ note-number ext-right) 0.0 0.0 peak 0.0 'dsl power-right fun-right fun-right)
  (loop for i from (- note-number ext-left) to (+ note-number ext-right) do
        (if (iget i 'sl) (iadd i  'sl (iget i 'dsl)))
        (rem-var (nth i *v*) 'dsl)
        ))


;;apply all marked accents in the score for dr
;; default 20% for quant or peak = 1
(defun accent-main-dr (quant)
  (each-note-if
   (this 'accent-dr)
   (then
    (let ((left (first (this 'accent-dr)))
          (right (second (this 'accent-dr)))
          (peak (third (this 'accent-dr)))
          (fun-left (fourth (this 'accent-dr)))
          (fun-right (fifth (this 'accent-dr)))
          function-left function-right power-left power-right)
      (multiple-value-setq (function-left power-left) (accents-translate-curv-name-left fun-left))
      (multiple-value-setq  (function-right power-right) (accents-translate-curv-name-right fun-right))
      (apply-accent-dr *i* left right (* quant peak 0.2) function-left function-right power-left power-right)
      ))))

;; Apply the IOI variations on the notes
;; first make 'ddr and then multiply that to 'dr
(defun apply-accent-dr (note-number ext-left ext-right peak fun-left fun-right power-left power-right)
  (iset-ramp-x2-decimal-last (- note-number ext-left) note-number 0.0 0.0 0.0 peak 'ddr power-left fun-left fun-left)
  (iset-ramp-x2-decimal-last note-number (+ note-number ext-right) 0.0 0.0 peak 0.0 'ddr power-right fun-right fun-right)
  (loop for i from (- note-number ext-left) to (+ note-number ext-right) do
         (iset i  'dr (* (iget i 'dr) (1+ (iget i 'ddr))))
        (rem-var (nth i *v*) 'ddr)
        ))


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


;;FUNCTIONS used for the shape of accents:
;;----power function
(defun power-fn-dec (x power)
  (expt x power))
(defun power-fn-acc (x power)
  (abs (expt (- x 1) power)))


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




    

