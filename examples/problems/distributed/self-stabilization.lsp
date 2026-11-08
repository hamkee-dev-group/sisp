; Dijkstra's self-stabilizing token ring. 4 processes, k=3 states.
; From ANY state, converge to exactly 1 token. Verify bounded steps.
(print "=== Self-Stabilizing Token Ring ===")
(seed 42)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

; 4 processes, each has state in {0,1,2} (k=3)
; States: list of 4 integers
(define K 3)
(define N 4)

; Bottom process (index 0): if S[0] == S[N-1], S[0] = (S[0]+1) mod K
(define (step-bottom states)
  (cond ((= (nth 0 states) (nth (- N 1) states))
         (list (mod (+ (nth 0 states) 1) K) (nth 1 states) (nth 2 states) (nth 3 states)))
        (t states)))

; Other processes (index i): if S[i] != S[i-1], S[i] = S[i-1]
(define (step-other states i)
  (cond ((not (= (nth i states) (nth (- i 1) states)))
         (cond ((= i 1) (list (nth 0 states) (nth (- i 1) states) (nth 2 states) (nth 3 states)))
               ((= i 2) (list (nth 0 states) (nth 1 states) (nth (- i 1) states) (nth 3 states)))
               ((= i 3) (list (nth 0 states) (nth 1 states) (nth 2 states) (nth (- i 1) states)))
               (t states)))
        (t states)))

; One full round: bottom then others
(define (one-round states)
  (step-other (step-other (step-other (step-bottom states) 1) 2) 3))

; Count tokens: a token is when bottom can move (S[0]==S[N-1]) OR S[i]!=S[i-1]
(define (has-token-at-bottom states) (= (nth 0 states) (nth (- N 1) states)))
(define (has-token-at-i states i) (not (= (nth i states) (nth (- i 1) states))))

(define (count-tokens states)
  (+ (cond ((has-token-at-bottom states) 1) (t 0))
     (cond ((has-token-at-i states 1) 1) (t 0))
     (cond ((has-token-at-i states 2) 1) (t 0))
     (cond ((has-token-at-i states 3) 1) (t 0))))

; Check if system is in legitimate state: exactly 1 token
(define (legitimate-p states) (= (count-tokens states) 1))

; Run until convergence or max steps. The round count is carried explicitly:
; deriving it as (- 30 budget) silently assumed every caller passed 30.
(define (converge-loop states budget used)
  (cond ((or (<= budget 0) (legitimate-p states))
         (list states (count-tokens states) used))
        (t (converge-loop (one-round states) (- budget 1) (+ used 1)))))

(define (run-until-legitimate states max-steps)
  (converge-loop states max-steps 0))

; --- Test 1: start from all same ---
(define s0 (list 0 0 0 0))
(define r0 (run-until-legitimate s0 30))
(print (list 'from-all-0 'final-state (car r0) 'tokens (cadr r0) 'steps (caddr r0)))
(ck "converge-from-uniform" (legitimate-p (car r0)))
(ck "bounded-convergence-uniform" (< (caddr r0) 30))

; --- Test 2: start from 2,1,0,2 ---
(define s1 (list 2 1 0 2))
(define r1 (run-until-legitimate s1 30))
(print (list 'from-2102 'final-state (car r1) 'tokens (cadr r1) 'steps (caddr r1)))
(ck "converge-from-2102" (legitimate-p (car r1)))
(ck "bounded-convergence-2102" (< (caddr r1) 30))

; --- Test 3: start from all tokens active ---
(define s2 (list 0 1 2 0))
(define r2 (run-until-legitimate s2 30))
(print (list 'from-0120 'final-state (car r2) 'tokens (cadr r2) 'steps (caddr r2)))
(ck "converge-from-all-active" (legitimate-p (car r2)))
(ck "bounded-convergence-0120" (< (caddr r2) 30))

; --- Test 4: exhaustive check from 16 sampdivd states ---
(print "Checking 16 sample states for convergence...")
(define test-states (list
  (list 0 0 0 0) (list 0 0 0 1) (list 0 0 1 0) (list 0 0 2 2)
  (list 0 1 0 1) (list 0 1 1 2) (list 0 2 0 1) (list 0 2 2 0)
  (list 1 0 0 2) (list 1 1 1 1) (list 1 2 0 1) (list 1 2 2 2)
  (list 2 0 1 0) (list 2 1 1 0) (list 2 2 0 0) (list 2 2 2 2)))

(define (check-list states-list)
  (cond ((null states-list) t)
        (t (let ((r (run-until-legitimate (car states-list) 30)))
             (and (legitimate-p (car r))
                  (check-list (cdr states-list)))))))

(define all-ok (check-list test-states))
(print (list 'all-16-converge all-ok))
(ck "sample-states-converge" all-ok)

; --- Test 5: EXHAUSTIVE. All K^N = 81 configurations must converge, and the
; worst case over all of them must fit inside the 30-round budget.
(define all-states
  (foldr append nil
    (map (lambda (a)
      (foldr append nil
        (map (lambda (b)
          (foldr append nil
            (map (lambda (c)
              (map (lambda (e) (list a b c e)) (seq 0 (- K 1))))
              (seq 0 (- K 1)))))
          (seq 0 (- K 1)))))
      (seq 0 (- K 1)))))

; Returns the worst-case round count, or -1 if any configuration fails to
; reach a legitimate state within the budget.
(define (worst-case lst acc)
  (cond ((null lst) acc)
        ((< acc 0) -1)
        (t (let ((r (run-until-legitimate (car lst) 30)))
             (cond ((legitimate-p (car r))
                    (worst-case (cdr lst) (max acc (caddr r))))
                   (t -1))))))

(define worst (worst-case all-states 0))
(print (list 'states-examined (length all-states) 'worst-case-rounds worst))
(ck "enumerated-all-81-states" (= (length all-states) 81))
(ck "every-state-converges" (>= worst 0))
; One full sweep (bottom, then processes 1..3) suffices from every configuration,
; so the exact worst case is 1 round — comfortably inside the 30-round budget.
(ck "worst-case-is-one-round" (= worst 1))
(ck "convergence-bounded-30" (< worst 30))

(print "=== Self-Stabilization Done ===")
