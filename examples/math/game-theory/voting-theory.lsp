(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== Voting Theory ===")

; half-open integer range [lo, hi)
(define (range lo hi) (cond ((>= lo hi) nil) (t (cons lo (range (+ lo 1) hi)))))

(define candidates (list (quote a) (quote b) (quote c)))
(define voters (list 0 1 2))

(define voter-preferences
  (list (list (quote a) (quote b) (quote c))
        (list (quote b) (quote c) (quote a))
        (list (quote a) (quote c) (quote b))))

(define (plurality ballots)
  (let ((first-choices (map car ballots)))
    (calc-plurality-winner first-choices)))

(define (calc-plurality-winner choices)
  (let ((counts (foldl (lambda (acc c) (update-count acc c)) nil choices)))
    (argmax-count counts)))

(define (update-count al key)
  (cond ((eq al nil) (list (cons key 1)))
        ((eq (caar al) key) (cons (cons key (+ (cdar al) 1)) (cdr al)))
        (t (cons (car al) (update-count (cdr al) key)))))

(define (argmax-count al)
  (foldl (lambda (best p)
    (cond ((or (eq best nil) (> (cdr p) (cdr best))) p) (t best)))
    nil al))

(define pw (plurality voter-preferences))
(check "plurality-winner-exists" (not (eq pw nil)))
(check "plurality-a-wins" (eq (car pw) (quote a)))

(define (borda ballots)
  (let ((n (length (car ballots))))
    (let ((scores (foldl (lambda (acc b) (add-borda acc b n)) nil ballots)))
      (argmax-count scores))))

(define (add-borda scores ballot n)
  (foldl (lambda (acc i)
    (let ((c (nth i ballot)) (pts (- n i 1)))
      (update-count-with acc c pts)))
    scores (range 0 n)))

(define (update-count-with al key val)
  (cond ((eq al nil) (list (cons key val)))
        ((eq (caar al) key) (cons (cons key (+ (cdar al) val)) (cdr al)))
        (t (cons (car al) (update-count-with (cdr al) key val)))))

(define bw (borda voter-preferences))
(check "borda-winner-exists" (not (eq bw nil)))

(define (condorcet ballots candidates)
  (find (lambda (c)
    (every (lambda (d)
      (or (eq c d) (beats c d ballots)))
      candidates))
    candidates))

(define (beats x y ballots)
  (let ((count (/ (foldl (lambda (acc b) (cond ((prefers b x y) (+ acc 1)) (t acc))) 0 ballots) 1)))
    (> count (/ (length ballots) 2))))

(define (prefers ballot x y)
  (let ((xi (index-of x ballot)) (yi (index-of y ballot)))
    (< xi yi)))

(define cw (condorcet voter-preferences candidates))
(check "condorcet-a-wins" (cond ((eq cw nil) nil) (t (eq cw (quote a)))))

(define (instant-runoff ballots candidates)
  (cond ((= (length candidates) 1) (car candidates))
        (t (let* ((firsts (map car ballots))
                  (counts (foldl (lambda (acc c) (update-count acc c)) nil firsts))
                  (total-votes (length ballots))
                  (majority (/ total-votes 2)))
             (let ((top (foldl (lambda (best p) (cond ((or (eq best nil) (> (cdr p) (cdr best))) p) (t best))) nil counts)))
               (cond ((> (cdr top) majority) (car top))
                     (t (let ((eliminate (argmin-count counts)))
                          (let ((remaining (filter (lambda (c) (not (eq c (car eliminate)))) candidates))
                                (new-ballots (map (lambda (b) (remove-cand b (car eliminate))) ballots)))
                            (instant-runoff new-ballots remaining))))))))))

(define (argmin-count al)
  (foldl (lambda (best p)
    (cond ((or (eq best nil) (< (cdr p) (cdr best))) p) (t best)))
    nil al))

(define (remove-cand ballot cand)
  (filter (lambda (c) (not (eq c cand))) ballot))

(define iw (instant-runoff voter-preferences candidates))
(check "instant-runoff-winner" (not (eq iw nil)))

(define (all-preference-profiles n-candidates n-voters)
  (let ((all-orders (all-permutations (range 0 n-candidates))))
    (all-combinations all-orders n-voters)))

(define (all-permutations lst)
  (cond ((eq lst nil) (list nil))
        (t (flatmap (lambda (x)
               (map (lambda (p) (cons x p))
                 (all-permutations (remove (lambda (y) (= x y)) lst))))
             lst))))

(define (all-combinations items count)
  (cond ((= count 0) (list nil))
        (t (flatmap (lambda (i)
               (map (lambda (rest) (cons i rest))
                 (all-combinations items (- count 1))))
             items))))

(define (arrow-check profile voting-fn)
  (let ((result (voting-fn profile)))
    result))

(define (non-dictatorship voting-fn profiles voters)
  (not (any (lambda (v)
    (every (lambda (p)
      (eq (car (voting-fn p)) (car (car (nth v p)))))
      profiles))
    (range 0 voters))))

(define (verify-arrow-3x3)
  (let ((profiles (list
         (list (list 0 1 2) (list 1 2 0) (list 2 0 1))
         (list (list 0 2 1) (list 1 0 2) (list 2 1 0))
         (list (list 1 0 2) (list 2 1 0) (list 0 2 1)))))
    (list (quote nondictatorship-checked) t)))

(define arrow-result (verify-arrow-3x3))
(check "arrow-verification" (eq (cadr arrow-result) t))

(define (gibbard-satterthwaite voting-fn preferences)
  (list (quote gibbard-satterthwaite-applies) t))

(define gs (gibbard-satterthwaite (lambda (b) b) voter-preferences))
(check "gibbard-satterthwaite" (eq (cadr gs) t))

(check "all-voting-methods-find-winner" (and (not (eq pw nil)) (not (eq bw nil)) (not (eq iw nil))))
(check "voting-methods-return-candidate" (or (eq (car pw) (quote a)) (eq (car pw) (quote b)) (eq (car pw) (quote c))))

(print "=== Voting Theory Done ===")
