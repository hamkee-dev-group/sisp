; ============================================================
; name: vector-clocks
; subject: data-structure
; capability: distributed causality
; status: healthy
; canonical-home: algorithms/data-structures/vector-clock.lsp
; notes: Textbook vector clocks for N = 3; the canonical version is
;        parameterised over N.
; ============================================================
; Vector clocks. send/recv. Happens-before. Detect concurrent writes.
(print "=== Vector Clocks ===")
(seed 42)

(define (ck name ok) (cond (ok (print (list 'pass name))) (t (print (list 'FAIL name)))))

(define N 3)

(define (mk-vc) (map (lambda (x) 0) (seq 1 N)))

(define (vc-inc vc id)
  (map (lambda (i) (cond ((= i id) (+ (nth i vc) 1)) (t (nth i vc)))) (seq 0 (- N 1))))

(define (vc-merge a b)
  (map (lambda (i) (max (nth i a) (nth i b))) (seq 0 (- N 1))))

; happens-before: a < b iff all a[i] <= b[i] and at least one <
(define (vc-lt-aux a b i le any)
  (cond ((= i N) (and le any))
        ((> (nth i a) (nth i b)) nil)
        ((< (nth i a) (nth i b)) (vc-lt-aux a b (+ i 1) le t))
        (t (vc-lt-aux a b (+ i 1) le any))))

(define (vc-lt a b) (vc-lt-aux a b 0 t nil))

(define (vc-concurrent a b)
  (and (not (vc-lt a b)) (not (vc-lt b a)) (not (equal a b))))

; Message: (sender-id clock payload)
(define (mk-msg sender clock payload) (list sender clock payload))
(define (msg-sender m) (car m))
(define (msg-clock m) (cadr m))
(define (msg-payload m) (caddr m))

; send: increment local clock, attach VC to message
(define (send vc sender payload)
  (let ((new-vc (vc-inc vc sender)))
    (list new-vc (mk-msg sender new-vc payload))))

; recv: increment local, merge with message clock
(define (recv vc msg receiver)
  (let ((inc-vc (vc-inc vc receiver)))
    (vc-merge inc-vc (msg-clock msg))))

; --- Test scenario ---
(define vc-a (mk-vc))
(define vc-b (mk-vc))

; P0 increments locally
(define vc-a1 (vc-inc vc-a 0))
(ck "local-inc-a0" (= (nth 0 vc-a1) 1))

; P0 sends msg to P1
(define s1 (send vc-a1 0 'hello))
(define vc-a2 (car s1))
(define msg1 (cadr s1))
(ck "send-increments-sender" (= (nth 0 vc-a2) 2))

; P1 receives msg
(define vc-b1 (recv vc-b msg1 1))
(ck "recv-merges-clocks" (= (nth 0 vc-b1) 2))
(ck "recv-increments-receiver" (= (nth 1 vc-b1) 1))

; P1 does local work
(define vc-b3 (vc-inc (vc-inc vc-b1 1) 1))

; P0 does concurrent local work
(define vc-a3 (vc-inc vc-a2 0))

; Happens-before checks
(ck "vc-a1-lt-vc-a2" (vc-lt vc-a1 vc-a2))
(ck "vc-a1-lt-vc-b1" (vc-lt vc-a1 vc-b1))
(ck "vc-a3-concurrent-vc-b3" (vc-concurrent vc-a3 vc-b3))

; P1 sends reply to P0
(define s2 (send vc-b3 1 'world))
(define vc-b4 (car s2))
(define msg2 (cadr s2))
(define vc-a4 (recv vc-a3 msg2 0))

(ck "vc-a2-lt-vc-a4" (vc-lt vc-a2 vc-a4))

; Concurrent write detection
(define va1 (vc-inc (mk-vc) 0))
(define vb1 (vc-inc (mk-vc) 1))
(ck "concurrent-detected" (vc-concurrent va1 vb1))

(print "=== Vector Clocks Done ===")
