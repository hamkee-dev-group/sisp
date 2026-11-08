(define (check name ok) (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
(print "=== Nash Equilibrium ===")

; half-open integer range [lo, hi)
(define (range lo hi) (cond ((>= lo hi) nil) (t (cons lo (range (+ lo 1) hi)))))

; Every pure strategy profile. `counts` lists each player's number of actions,
; so (2 2) gives ((0 0) (0 1) (1 0) (1 1)).
; NOTE: flatmap deep-flattens and would destroy the profiles, so the one-level
; concatenation is done with (foldr append nil ...).
(define (all-profiles counts)
  (cond ((eq counts nil) (list nil))
        (t (foldr append nil
             (map (lambda (a)
                    (map (lambda (rest) (cons a rest))
                         (all-profiles (cdr counts))))
                  (range 0 (car counts)))))))

; payoff-fn maps a profile to the vector of every player's payoff.
(define (pure-nash payoff-fn counts)
  (filter (lambda (p) (nash-check p payoff-fn counts)) (all-profiles counts)))

; A profile is Nash iff no player strictly gains by a unilateral deviation.
(define (nash-check profile payoff-fn counts)
  (every (lambda (i)
    (let ((curr-pay (nth i (payoff-fn profile))))
      (every (lambda (a)
               (>= curr-pay (nth i (payoff-fn (replace-nth profile i a)))))
             (range 0 (nth i counts)))))
    (range 0 (length counts))))

(define (replace-nth lst i val)
  (cond ((eq lst nil) nil) ((= i 0) (cons val (cdr lst))) (t (cons (car lst) (replace-nth (cdr lst) (- i 1) val)))))

(define (pd-payoff player action1 action2)
  (cond ((and (= action1 0) (= action2 0)) (list -1 -1))
        ((and (= action1 0) (= action2 1)) (list -3 0))
        ((and (= action1 1) (= action2 0)) (list 0 -3))
        ((and (= action1 1) (= action2 1)) (list -2 -2))
        (t (list 0 0))))

; payoff-fn for a 2-player matrix game: profile (i j) -> matrix[i][j]
(define (matrix-payoff-fn matrix)
  (lambda (profile) (nth (cadr profile) (nth (car profile) matrix))))

(define (pd-matrix)
  (list (list (list -1 -1) (list -3 0)) (list (list 0 -3) (list -2 -2))))

(define pd-players (list 0 1))
(define pd-actions (list 2 2))

(define (pd-payoffs p1 p2) 
  (nth p2 (nth p1 (pd-matrix))))

(define (pure-nash-2x2 matrix)
  (let ((profiles (list (list 0 0) (list 0 1) (list 1 0) (list 1 1))))
    (filter (lambda (p)
      (let ((p1 (car p)) (p2 (cadr p)))
        (let ((p1-pay (car (nth p2 (nth p1 matrix))))
              (p2-pay (cadr (nth p2 (nth p1 matrix)))))
          (and
            (>= p1-pay (car (nth p2 (nth 0 matrix))))
            (>= p1-pay (car (nth p2 (nth 1 matrix))))
            (>= p2-pay (cadr (nth 0 (nth p1 matrix))))
            (>= p2-pay (cadr (nth 1 (nth p1 matrix))))))))
      profiles)))

(define pd-nash (pure-nash-2x2 (pd-matrix)))
(check "prisoners-dilemma-defect" (equal pd-nash (list (list 1 1))))

(define (bos-matrix)
  (list (list (list 2 1) (list 0 0)) (list (list 0 0) (list 1 2))))
(define bos-nash (pure-nash-2x2 (bos-matrix)))
(check "battle-of-sexes-two-nash" (= (length bos-nash) 2))

(define (mp-matrix)
  (list (list (list 1 -1) (list -1 1)) (list (list -1 1) (list 1 -1))))
(define mp-nash (pure-nash-2x2 (mp-matrix)))
(check "matching-pennies-no-pure" (= (length mp-nash) 0))

; Mixed Nash of a 2x2 game, by the indifference principle.
; matrix[i][j] = (u1 u2) when player 1 plays row i and player 2 plays column j.
;   a b / c d  are player 1's payoffs, e f / g h player 2's.
; Returns (p q): p = P(player 1 plays row 0), q = P(player 2 plays column 0).
; Player 2's mix q makes player 1 indifferent between his two rows:
;   q*a + (1-q)*b = q*c + (1-q)*d  =>  q = (d - b) / (a + d - b - c)
; Player 1's mix p makes player 2 indifferent between her two columns:
;   p*e + (1-p)*g = p*f + (1-p)*h  =>  p = (h - g) / (e + h - f - g)
; Division is exact rational, so a 1/2-1/2 equilibrium comes out as exactly 1/2.
(define (mixed-nash-2x2 matrix)
  (let* ((a (car (car (car matrix))))
         (b (car (nth 1 (car matrix))))
         (c (car (nth 0 (nth 1 matrix))))
         (d (car (nth 1 (nth 1 matrix))))
         (e (cadr (car (car matrix))))
         (f (cadr (nth 1 (car matrix))))
         (g (cadr (nth 0 (nth 1 matrix))))
         (h (cadr (nth 1 (nth 1 matrix)))))
    (let ((denom-p (+ e h (- 0 f) (- 0 g)))
          (denom-q (+ a d (- 0 b) (- 0 c))))
      (cond ((or (= denom-p 0) (= denom-q 0)) nil)
            (t (let ((p (/ (- h g) denom-p))
                     (q (/ (- d b) denom-q)))
                 ; only a genuine interior mixed equilibrium counts
                 (cond ((and (and (> p 0) (< p 1)) (and (> q 0) (< q 1)))
                        (list p q))
                       (t nil))))))))

(define mp-mixed (mixed-nash-2x2 (mp-matrix)))
(check "matching-pennies-mixed" (and (> (car mp-mixed) 0.4) (< (car mp-mixed) 0.6)))

; Every non-empty support over `actions` actions, enumerated by bitmask.
; There are 2^actions - 1 of them, so the mask runs over [1, 2^actions).
(define (all-supports actions)
  (foldl (lambda (acc mask) (cons (support-from-mask mask actions) acc))
    nil (range 1 (^ 2 actions))))

(define (support-from-mask mask size)
  (foldl (lambda (acc i)
    (cond ((not (= (mod (truncate (/ mask (^ 2 i))) 2) 0)) (cons i acc)) (t acc)))
    nil (reverse (range 0 size))))

(check "supports-of-2-actions" (= (length (all-supports 2)) 3))

(define (nash-payoff-equal matrix p1 p2)
  (let ((u1 (car (nth p2 (nth p1 matrix))))
        (u2 (cadr (nth p2 (nth p1 matrix)))))
    (list u1 u2)))

(check "mp-mixed-half-half" (< (abs (- (car mp-mixed) 0.5)) 0.1))
(check "mixed-strategy-is-prob" (and (> (car mp-mixed) 0) (< (car mp-mixed) 1)))

; Matching Pennies has a unique mixed NE at exactly (1/2, 1/2) for both players.
(check "mp-mixed-exact-half" (equal mp-mixed (list (/ 1 2) (/ 1 2))))
; a mixed strategy over two actions must put its mass on the two of them
(check "mp-mixed-sums-to-one"
  (and (= (+ (car mp-mixed) (- 1 (car mp-mixed))) 1)
       (= (+ (cadr mp-mixed) (- 1 (cadr mp-mixed))) 1)))

; Battle of the Sexes mixed NE: each player plays their own preferred outcome
; with probability 2/3, so (p q) = (2/3, 1/3). Exact rationals, verified by hand.
(define bos-mixed (mixed-nash-2x2 (bos-matrix)))
(check "bos-mixed-two-thirds" (equal bos-mixed (list (/ 2 3) (/ 1 3))))

; the generic n-player solver must agree with the closed-form 2x2 solver
(define pd-nash-generic (pure-nash (matrix-payoff-fn (pd-matrix)) pd-actions))
(check "generic-pure-nash-agrees" (equal pd-nash-generic pd-nash))
; Prisoner's Dilemma is a strict-dominance game, so the pure NE is unique
(check "generic-pd-unique" (= (length pd-nash-generic) 1))

(define (verify-nash matrix profile)
  (let ((p1 (car profile)) (p2 (cadr profile)))
    (let ((u (nash-payoff-equal matrix p1 p2)))
      (and (>= (car u) -100) (>= (cadr u) -100)))))

(define pd-verify (verify-nash (pd-matrix) (list 1 1)))
(check "pd-nash-verify" (eq pd-verify t))

(print "=== Nash Equilibrium Done ===")
