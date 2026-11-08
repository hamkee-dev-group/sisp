; ============================================================
; name: vector-clock
; file: algorithms/data-structures/vector-clock.lsp
; subject: data-structure
; capability: distributed causality happens-before
; aliases: "vector time" "logical clock"
; signature: (vc-new n) (vc-inc vc id) (vc-merge a b) (vc-lt a b)
;            (vc-concurrent a b) (vc-send vc id payload)
;            (vc-recv vc msg id)
; complexity: O(n) per operation, n = number of processes
; see-also: crdt-gcounter
; derived-from: Fidge 1988; Mattern 1989; see also
;               examples/problems/distributed/vector-clocks.lsp
; status: example (tested)
; checks: 12 checks; run: src/sisp <this file> from repo root
; ============================================================
; Vector clocks (Fidge/Mattern): a list of per-process counters.
; Local events increment the owner's counter; a receive increments the
; receiver's counter and merges elementwise max.  a happens-before b
; (vc-lt) iff every a[i] <= b[i] and at least one is strictly less.
; Two clocks are concurrent when neither happens-before the other.

(define (vc-new n)
  (cond ((<= n 0) (error "VC-NEW: N MUST BE POSITIVE" n))
        (t (map (lambda (i) 0) (seq 1 n)))))

(define (vc-inc vc id)
  (map (lambda (i)
         (cond ((= i id) (+ (nth i vc) 1)) (t (nth i vc))))
       (seq 0 (- (length vc) 1))))

(define (vc-merge a b)
  (map (lambda (i) (max (nth i a) (nth i b)))
       (seq 0 (- (length a) 1))))

(define (vc-lt-aux a b i le any)
  (cond ((= i (length a)) (and le any))
        ((> (nth i a) (nth i b)) nil)
        ((< (nth i a) (nth i b)) (vc-lt-aux a b (+ i 1) le t))
        (t (vc-lt-aux a b (+ i 1) le any))))

(define (vc-lt a b)
  "t when a happens-before b"
  (vc-lt-aux a b 0 t nil))

(define (vc-concurrent a b)
  "t when neither clock happens-before the other"
  (and (not (vc-lt a b)) (not (vc-lt b a)) (not (equal a b))))

; message = (sender clock payload)
(define (vc-send vc id payload)
  "send a message: increment local clock, stamp the message"
  (let ((new-vc (vc-inc vc id)))
    (list new-vc (list id new-vc payload))))

(define (vc-recv vc msg id)
  "receive a message: increment local clock, merge with the stamp"
  (vc-merge (vc-inc vc id) (cadr msg)))

; ---- checks ----

(check "vector-clock: new clock is all zeros"
       (equal (vc-new 3) (list 0 0 0)))

(check "vector-clock: local increment advances one counter"
       (equal (vc-inc (vc-new 3) 1) (list 0 1 0)))

(check "vector-clock: send increments the sender"
       (let ((s (vc-send (vc-new 2) 0 "hello")))
         (and (equal (car s) (list 1 0))
              (equal (cadr s) (list 0 (list 1 0) "hello")))))

(check "vector-clock: recv merges elementwise max and increments"
       (let* ((s (vc-send (vc-inc (vc-new 2) 0) 0 "m"))
              (received (vc-recv (vc-new 2) (cadr s) 1)))
         (equal received (list 2 1))))

(check "vector-clock: same clock is not happens-before"
       (eq (vc-lt (list 1 1) (list 1 1)) nil))

(check "vector-clock: strict dominance is happens-before"
       (and (vc-lt (list 1 0) (list 2 1))
            (vc-lt (list 0 0) (list 1 0))
            (not (vc-lt (list 2 1) (list 1 0)))))

(check "vector-clock: concurrent writes detected"
       (and (vc-concurrent (list 1 0) (list 0 1))
            (not (vc-concurrent (list 1 0) (list 2 1)))))

(check "vector-clock: merge is commutative"
       (equal (vc-merge (list 1 3 0) (list 2 1 4))
              (vc-merge (list 2 1 4) (list 1 3 0))))

(check "vector-clock: merge takes elementwise max"
       (equal (vc-merge (list 1 3 0) (list 2 1 4)) (list 2 3 4)))

(check "vector-clock: happens-before is transitive"
       (and (vc-lt (list 1 0) (list 2 0))
            (vc-lt (list 2 0) (list 2 1))
            (vc-lt (list 1 0) (list 2 1))))

(check "vector-clock: causal chain through messages"
       (let* ((sa (vc-send (vc-new 2) 0 "m1"))
              (va (car sa))
              (vb (vc-recv (vc-new 2) (cadr sa) 1))
              (sb (vc-send vb 1 "m2")))
         ; m2's clock dominates m1's clock: m1 happens-before m2
         (and (vc-lt (cadr (cadr sa)) (cadr (cadr sb)))
              (not (vc-concurrent (cadr (cadr sa)) (cadr (cadr sb)))))))

(check "vector-clock: forked histories are concurrent"
       (let* ((base (vc-new 2))
              (left (car (vc-send base 0 "a")))
              (right (car (vc-send base 1 "b"))))
         (vc-concurrent left right)))
