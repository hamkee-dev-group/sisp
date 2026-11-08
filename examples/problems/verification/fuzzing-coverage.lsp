; AFL-style fuzzing. Instrument parser, track edge coverage bitmap.
; Mutate inputs (bit flips, splicing). Report new paths discovered.
(print "=== Fuzzing Coverage ===")
(seed 42)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

; --- Sparse coverage set of discovered edge identifiers ---
(setq *coverage* nil)
(setq *new-paths* 0)

; --- Simple arithmetic expression parser (instrumented) ---
(define (is-digit c)
  (let ((n (str-to-num c)))
    (and (numberp n) (>= n 0) (<= n 9))))
(define (is-op c)
  (or (equal c "+") (equal c "-") (equal c "*") (equal c "/")))

(define (tokenize s)
  (tok s 0 nil))

(define (tok s pos acc)
  (cond ((>= pos (length s)) (reverse acc))
        ((equal (substr pos 1 s) " ") (tok s (+ pos 1) acc))
        ((is-digit (substr pos 1 s))
         (let ((end (num-end s pos)))
           (tok s end (cons (substr pos (- end pos) s) acc))))
        (t (tok s (+ pos 1) (cons (substr pos 1 s) acc)))))

(define (num-end s pos)
  (cond ((and (< pos (length s)) (is-digit (substr pos 1 s)))
         (num-end s (+ pos 1)))
        (t pos)))

; Parse expression, returning (value . trace-list)
(define (parse-expr tokens pos trace)
  (let ((left (parse-term tokens pos (cons 0 trace))))
    (let ((lv (car left)) (lp (cadr left)) (lt (caddr left)))
      (cond ((and (< lp (length tokens)) (is-op (nth lp tokens)))
             (let* ((op (nth lp tokens))
                    (right (parse-expr tokens (+ lp 1) (cons 7 lt)))
                    (rv (car right)) (rp (cadr right)) (rt (caddr right)))
               (cond ((equal op "+") (list (+ lv rv) rp (cons 8 rt)))
                     ((equal op "-") (list (- lv rv) rp (cons 8 rt)))
                     ((equal op "*") (list (* lv rv) rp (cons 8 rt)))
                     ((equal op "/")
                      (cond ((= rv 0) (list 0 rp (cons 8 rt)))
                            (t (list (/ lv rv) rp (cons 8 rt)))))
                     (t (list lv rp (cons 9 rt))))))
            (t (list lv lp (cons 9 lt)))))))

(define (parse-term tokens pos trace)
  (let ((token (nth pos tokens)))
    (cond ((null token) (list 0 pos (cons 1 trace)))
          ((is-digit (substr 0 1 token))
           (list (str-to-num token) (+ pos 1) (cons 2 trace)))
          ((equal token "(")
           (let ((inner (parse-expr tokens (+ pos 1) (cons 3 trace))))
             (let ((v (car inner)) (np (cadr inner)) (tt (caddr inner)))
               (cond ((and (< np (length tokens)) (equal (nth np tokens) ")"))
                      (list v (+ np 1) (cons 4 tt)))
                     (t (list v np (cons 5 tt)))))))
          (t (list 0 pos (cons 6 trace))))))

; Record edge coverage
(define (record-coverage trace)
  (cond ((or (null trace) (null (cdr trace))) nil)
        (t (let ((edge (+ (* (car trace) 256) (cadr trace))))
             (cond ((not (contains edge *coverage*))
                    (setq *new-paths* (+ *new-paths* 1))
                    (setq *coverage* (cons edge *coverage*))))
             (record-coverage (cdr trace))))))

; Parse with coverage instrumentation
(define (parse-covered s)
  (let ((tokens (tokenize s)))
    (let ((result (parse-expr tokens 0 nil)))
      (let ((trace (caddr result)))
        (record-coverage trace)
        (car result)))))

; --- Mutation strategies ---
(define (byte-flip s pos)
  (cond ((>= pos (length s)) s)
        (t (let* ((c (substr pos 1 s))
                  (digit (str-to-num c))
                  (new-c (cond ((equal c "+") "-")
                               ((equal c "-") "*")
                               ((equal c "*") "/")
                               ((equal c "/") "+")
                               ((numberp digit) (num-to-str (mod (+ digit 1) 10)))
                               (t c))))
             (cat (cat (substr 0 pos s) new-c)
                  (substr (+ pos 1) (- (length s) (+ pos 1)) s))))))

(define (splice a b pos)
  (cond ((> pos (length a)) a)
        (t (cat (substr 0 pos a)
                (substr 0 (min (length b) (- (length a) pos)) b)))))

; --- Fuzzing loop ---
(define (fuzz inputs generations)
  (fuzz-loop inputs inputs generations))

(define (fuzz-loop queue all-inputs gens)
  (cond ((<= gens 0) (list *new-paths* queue))
        (t (let ((new-inputs (try-mutations queue all-inputs gens)))
             (fuzz-loop (append queue new-inputs) all-inputs (- gens 1))))))

(define (try-mutations queue all-inputs gen)
  (try-inputs queue all-inputs gen nil))

(define (try-inputs queue all-inputs gen acc)
  (cond ((null queue) acc)
        (t (let ((inp (car queue)))
             (let ((mutated (try-positions inp 0 (length inp) nil)))
               (let ((spliced (cond ((> (length all-inputs) 1)
                                     (let ((other (nth (mod gen (length all-inputs)) all-inputs)))
                                       (splice inp other (mod gen (length inp)))))
                                    (t inp))))
                 (let ((paths-before *new-paths*))
                   (cond ((not (equal spliced inp))
                          (parse-covered spliced))
                         (t nil))
                   (try-inputs (cdr queue) all-inputs gen
                     (append acc (cond ((not (equal spliced inp)) (list spliced)) (t nil))
                             mutated)))))))))

(define (try-positions inp pos end acc)
  (cond ((>= pos end) acc)
        (t (let ((mut (byte-flip inp pos)))
             (cond ((not (equal mut inp))
                    (let ((before *new-paths*))
                      (parse-covered mut)
                      (try-positions inp (+ pos 1) end
                        (cond ((> *new-paths* before) (cons mut acc)) (t acc)))))
                   (t (try-positions inp (+ pos 1) end acc)))))))

; --- Run fuzzer ---
(define seed-inputs (list "1+2" "3*4" "5-1" "10/2" "(1+2)*3" "4+5+6" "7*(8+9)" "100-50/2" "1" "99"))

(print "Initial seed corpus:")
(print seed-inputs)

; Parse all seeds to establish baseline coverage
(foreach (lambda (inp) (parse-covered inp)) seed-inputs)
(print (list 'baseline-paths *new-paths*))

; Fuzz for 5 generations
(define fuzz-result (fuzz seed-inputs 5))
(print (list 'total-paths-discovered (car fuzz-result)))

(ck "coverage-increases" (> (car fuzz-result) 0))
(ck "fuzzer-discovered-paths" (> (car fuzz-result) 3))

; --- Edge coverage density ---
(define (coverage-density)
  (length *coverage*))

(define density (coverage-density))
(print (list 'unique-edges-covered density))
(ck "edges-covered-positive" (> density 0))

; --- Test specific mutation ---
(define test-input "1+2")
(define byte-flipped (byte-flip "1+2" 1))
(print (list 'byte-flip "1+2" byte-flipped))
(ck "byte-flip-works" (not (equal byte-flipped test-input)))

(print "=== Fuzzing Coverage Done ===")
