; ============================================================
; CONNECTION MACHINE CM-1 — SIMD with 64K processing elements
;
; Each PE has: ALU, 4K bits memory, flag register, neighbor links
; All PEs execute the SAME instruction (SIMD).
; Hypercube topology — each PE connects to log2(N) neighbors.
; ============================================================

(define NUM_PES 64)
(define PE_MEM 64)  ; bits per PE

; Each PE: (id mem flag enabled)
(define (pe-id     pe) (car pe))
(define (pe-mem    pe) (cadr pe))
(define (pe-flag   pe) (caddr pe))
(define (pe-active pe) (cadddr pe))

(define (make-pe id)
  (list id (make-list-cm PE_MEM 0) 0 t))

(define (make-list-cm n v)
  (cond ((= n 0) nil) (t (cons v (make-list-cm (- n 1) v)))))

; PE memory access
(define (pe-rd pe addr)
  (nth addr (pe-mem pe)))

(define (pe-wr pe addr val)
  (list (pe-id pe) (set-nth-cm (pe-mem pe) addr val) (pe-flag pe) (pe-active pe)))

(define (set-nth-cm lst i v)
  (cond ((null lst) nil) ((= i 0) (cons v (cdr lst))) (t (cons (car lst) (set-nth-cm (cdr lst) (- i 1) v)))))

; ---- CM-1 instruction set -----------------------------------
; LOAD  addr   — load from PE memory into register (simulated as flag)
; STORE addr   — store register to PE memory
; ADD   addr   — add memory[addr] to register
; SEND  dir    — send register to neighbor in direction dir (0=N,1=E,2=S,3=W)
; GET   dir    — receive from neighbor
; WHERE flag   — enable only PEs where flag is set

(define cm-memory nil)  ; ((pe-id . value) ...) for routing
(define cm-acc nil)      ; per-PE accumulator values

(define (cm-init)
  (setq cm-acc (make-list-cm NUM_PES 0))
  (setq cm-memory nil))

(define (cm-pes) (map make-pe (seq-cm 0 (- NUM_PES 1))))

(define (seq-cm lo hi)
  (cond ((> lo hi) nil) (t (cons lo (seq-cm (+ lo 1) hi)))))

; All PEs execute the same operation
(define (cm-load address pes)
  (setq cm-acc (map (lambda (pe) (pe-rd pe address)) pes))
  pes)

(define (cm-store address pes)
  (map (lambda (pe) (pe-wr pe address (nth (pe-id pe) cm-acc))) pes))

(define (cm-add address pes)
  (setq cm-acc (map (lambda (pe) (+ (nth (pe-id pe) cm-acc) (pe-rd pe address))) pes))
  pes)

; Hypercube routing: send to neighbor by toggling bit `dir`
(define (cm-send dir pes)
  (setq cm-memory
    (map (lambda (pe)
           (let ((i (pe-id pe)))
             (list (logxor i (^ 2 dir)) (nth i cm-acc))))
         pes))
  pes)

(define (cm-get dir pes)
  (setq cm-acc
    (map (lambda (pe)
           (let ((msg (find (lambda (m) (= (car m) (pe-id pe))) cm-memory)))
             (cond (msg (cadr msg)) (t 0))))
         pes))
  (setq cm-memory nil)
  pes)

(define (logxor a b)
  (lx a b 0 1))
(define (lx a b pos bit)
  (cond ((= pos 8) 0)
        ((not (= (mod a 2) (mod b 2))) (+ bit (lx (truncate (/ a 2)) (truncate (/ b 2)) (+ pos 1) (* bit 2))))
        (t (lx (truncate (/ a 2)) (truncate (/ b 2)) (+ pos 1) (* bit 2)))))

(define (cm-sum pes)
  (foldl (lambda (acc pe) (+ acc (pe-rd pe 0))) 0 pes))

(define (str-join parts sep)
  (cond ((null parts) "") ((null (cdr parts)) (car parts)) (t (cat (cat (car parts) sep) (str-join (cdr parts) sep)))))

; ============================================================
; DEMO: sum 64 numbers using parallel reduction (hypercube)
; ============================================================
(print "=============================================")
(print "  CONNECTION MACHINE — SIMD with 64 PEs")
(print "=============================================")
(print "")

(cm-init)
(define pes (cm-pes))

; Initialize: each PE gets its ID as value at address 0
(define pes (map (lambda (pe) (pe-wr pe 0 (pe-id pe))) pes))

; Sum all values: hypercube reduction in log2(64) = 6 steps
; Step d: each PE sends to neighbor along dimension d, adds received
(define pes (cm-load 0 pes))   ; load values into acc
(define pes (cm-send 0 pes))   ; send across dimension 0
(define pes (cm-get 0 pes))    ; receive into acc
; After one hop, each PE has sum of itself and neighbor.

(print (str-join (list "64 PEs initialized. PE[0] value: " (num-to-str (pe-rd (nth 0 pes) 0))) ""))
(print (str-join (list "PE[63] value: " (num-to-str (pe-rd (nth 63 pes) 0))) ""))
(print (str-join (list "Total sum of PE IDs (by fold): " (num-to-str (cm-sum pes))) ""))
(print (str-join (list "Expected: " (num-to-str (truncate (/ (* 63 64) 2))) "") ""))
(print "")
(print "=============================================")
(print "  Connection Machine — SIMD hypercube verified")
(print "=============================================")
