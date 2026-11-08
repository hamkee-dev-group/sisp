; synthesis.lsp — Additive synthesis. FM synthesis.
; Wavetable synthesis. Generate simple tones and chords.
(print "=== Synthesis ===")

(define PI 3.141592653589793)
(define EPS 0.000000001)

(define (close-p a b) (< (abs (- a b)) EPS))

; Sample rate and duration constants
(define SR 44100)
(define DUR 1.0)

; --- additive synthesis: a sum of sinusoids -------------------------------
(define (additive-synth freqs amps dur sr)
  (let ((nsamps (floor (* dur sr))))
    (additive-gen freqs amps nsamps sr 0 nil)))

(define (additive-gen freqs amps nsamps sr i acc)
  (cond ((>= i nsamps) (reverse acc))
        (t (let ((tv (/ i sr)))
          (additive-sample freqs amps tv 0.0 0 acc sr nsamps i)))))

(define (additive-sample freqs amps tv sum j acc sr nsamps i)
  (cond ((>= j (length freqs))
         (additive-gen freqs amps nsamps sr (+ i 1) (cons sum acc)))
        (t (additive-sample freqs amps tv
               (+ sum (* (nth j amps) (sin (* 2.0 PI (nth j freqs) tv))))
               (+ j 1) acc sr nsamps i))))

; An A major chord: A4 = 440, C#5 = 554.37, E5 = 659.25 Hz.
(define chord-freqs (list 440.0 554.37 659.25))
(define chord-amps (list 0.3 0.3 0.3))
(define chord (additive-synth chord-freqs chord-amps 0.01 SR))
(print "Chord: first sample =" (car chord) " last =" (last chord))

; 0.01 s at 44100 Hz is exactly 441 samples.
(check "additive-length" (= (length chord) 441))

; Every partial starts at phase 0, so sample 0 is exactly 0.
(check "additive-starts-at-zero" (close-p (car chord) 0.0))

; Independent re-derivation of one sample: 0.3*(sin(2*PI*440*t) +
; sin(2*PI*554.37*t) + sin(2*PI*659.25*t)) at t = 100/44100.
(define (chord-ref i)
  (let ((tv (/ i SR)))
    (* 0.3 (+ (sin (* 2.0 PI 440.0 tv))
              (sin (* 2.0 PI 554.37 tv))
              (sin (* 2.0 PI 659.25 tv))))))

(check "additive-sample-100" (close-p (nth 100 chord) (chord-ref 100)))
(check "additive-sample-value" (close-p (nth 100 chord) 0.30504498768468075))

; Three partials of amplitude 0.3 can never exceed 0.9 in magnitude.
(define (all-below l bound)
  (cond ((eq l nil) t)
        ((< (abs (car l)) bound) (all-below (cdr l) bound))
        (t nil)))

(check "additive-bounded" (all-below chord 0.9000000001))

; --- FM synthesis: y(t) = sin(2*PI*fc*t + I*sin(2*PI*fm*t)) ---------------
(define (fm-synth carrier mod-freq index dur sr)
  (let ((nsamps (floor (* dur sr))))
    (fm-gen carrier mod-freq index nsamps sr 0 nil)))

(define (fm-gen fc fmod idx nsamps sr i acc)
  (cond ((>= i nsamps) (reverse acc))
        (t (let* ((tv (/ i sr))
                  (phase (+ (* 2.0 PI fc tv) (* idx (sin (* 2.0 PI fmod tv)))))
                  (y (sin phase)))
          (fm-gen fc fmod idx nsamps sr (+ i 1) (cons y acc))))))

(define fm-tone (fm-synth 220.0 110.0 3.0 0.005 SR))
(print "FM tone: first =" (car fm-tone) " sample 50 =" (nth 50 fm-tone))

; 0.005 s at 44100 Hz is exactly 220 samples (floor of 220.5).
(check "fm-length" (= (length fm-tone) 220))
; Both the carrier and the modulator start at phase 0, so y(0) = sin(0) = 0.
(check "fm-starts-at-zero" (close-p (car fm-tone) 0.0))
; sin(2*PI*220*t + 3*sin(2*PI*110*t)) at t = 50/44100.
(check "fm-sample-50" (close-p (nth 50 fm-tone) -0.5168615829249464))
; A sine of any phase stays within [-1,1].
(check "fm-bounded" (all-below fm-tone 1.0000000001))

; --- wavetable synthesis --------------------------------------------------
; Precompute one cycle of a sine, then read it back at a variable rate with
; linear interpolation between neighbouring table entries.
(define (make-wavetable size)
  (wavetable-fill size 0 nil))

(define (wavetable-fill size i acc)
  (cond ((>= i size) (reverse acc))
        (t (let ((val (sin (* 2.0 PI (/ i size)))))
          (wavetable-fill size (+ i 1) (cons val acc))))))

(define (wavetable-synth table freq dur sr)
  (let* ((nsamps (floor (* dur sr)))
         (tsize (length table))
         (incr (/ (* freq tsize) sr)))
    (wavetable-gen table incr tsize nsamps 0.0 0 nil)))

; The phase may be an exact rational, and `mod` only accepts integers, so the
; wrap is done by subtraction (valid because incr < tsize for any freq < sr).
(define (wrap-phase p tsize)
  (cond ((>= p tsize) (- p tsize))
        (t p)))

(define (wavetable-gen table incr tsize nsamps phase i acc)
  (cond ((>= i nsamps) (reverse acc))
        (t (let* ((idx (floor phase))
                  (frac (- phase idx))
                  (v1 (nth (mod idx tsize) table))
                  (v2 (nth (mod (+ idx 1) tsize) table))
                  (y (+ v1 (* frac (- v2 v1)))))
          (wavetable-gen table incr tsize nsamps
                        (wrap-phase (+ phase incr) tsize) (+ i 1) (cons y acc))))))

(define tbl (make-wavetable 256))
(print "Table: [0] =" (nth 0 tbl) " [64] =" (nth 64 tbl) " [192] =" (nth 192 tbl))
(check "wavetable-size" (= (length tbl) 256))
; A quarter of the way round the cycle is sin(PI/2) = 1, three quarters is -1.
(check "wavetable-quarter" (close-p (nth 64 tbl) 1.0))
(check "wavetable-three-quarter" (close-p (nth 192 tbl) -1.0))

; Playing the table at sr/size Hz gives a phase increment of exactly one, so
; the output must reproduce the table entry for entry.
(define unit-rate (/ (* 1.0 SR) 256))
(define wt-unit (wavetable-synth tbl unit-rate 0.001 SR))

(define (lists-close l1 l2)
  (cond ((and (eq l1 nil) (eq l2 nil)) t)
        ((or (eq l1 nil) (eq l2 nil)) nil)
        ((close-p (car l1) (car l2)) (lists-close (cdr l1) (cdr l2)))
        (t nil)))

(check "wavetable-unit-rate" (lists-close wt-unit (take tbl (length wt-unit))))

(define wt-tone (wavetable-synth tbl 440.0 0.005 SR))
(print "Wavetable 440Hz: [0] =" (car wt-tone) " length =" (length wt-tone))
(check "wavetable-length" (= (length wt-tone) 220))
(check "wavetable-starts-at-zero" (close-p (car wt-tone) 0.0))

; --- pure tone reference --------------------------------------------------
(define (pure-tone freq dur sr)
  (let ((nsamps (floor (* dur sr))))
    (pure-tone-gen freq nsamps sr 0 nil)))

(define (pure-tone-gen freq nsamps sr i acc)
  (cond ((>= i nsamps) (reverse acc))
        (t (pure-tone-gen freq nsamps sr (+ i 1)
                          (cons (sin (* 2.0 PI freq (/ i sr))) acc)))))

(define tone (pure-tone 440.0 0.005 SR))
(print "Pure 440Hz tone, sample 0 =" (car tone) " sample 10 =" (nth 10 tone))
(check "pure-tone-zero" (close-p (car tone) 0.0))
; sin(2*PI*440*10/44100) = sin(0.6268937721...) = 0.5866320022005456
(check "pure-tone-sample-10" (close-p (nth 10 tone) 0.5866320022005456))

; A 256-point table with linear interpolation tracks the true sine to well
; under 1e-4 in absolute error.
(define (max-abs-diff l1 l2 worst)
  (cond ((or (eq l1 nil) (eq l2 nil)) worst)
        (t (max-abs-diff (cdr l1) (cdr l2) (max worst (abs (- (car l1) (car l2))))))))

(define wt-err (max-abs-diff wt-tone tone 0.0))
(print "Wavetable vs exact sine, max error =" wt-err)
(check "wavetable-tracks-sine" (< wt-err 0.0001))

(print "=== Synthesis Done ===")
