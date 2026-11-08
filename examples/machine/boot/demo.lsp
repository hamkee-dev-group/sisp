; ============================================================
; SISP MACHINE DEMO — Fullstack: Forth -> CPU -> Output
; Proves the stack: compiler, assembler, executor all in SISP.
; ============================================================

(define WORD 16777216) (define MEM 256) (define NREG 11) (define PCI 8) (define HLI 9) (define SP 6)

(define (rd m a) (truncate (mat-ref (car m) 0 a)))
(define (wr m a v) (mat-set (car m) 0 a (mod v WORD)))
(define (gr m r) (truncate (mat-ref (car (cdr m)) 0 r)))
(define (sr m r v) (mat-set (car (cdr m)) 0 r (mod v WORD)))
(define (mk) (list (make-matrix 1 MEM) (make-matrix 1 NREG)))

(define (spush m v) (let ((s (- (gr m SP) 1))) (sr m SP s) (wr m s v)))
(define (spop  m)   (let ((s (gr m SP))) (let ((v (rd m s))) (sr m SP (+ s 1)) v)))
(define (iw op a b) (+ (* op 65536) (* a 256) b))
(define (cat2 a b) (cat a b))

(define (exec m w)
  (let ((op (mod (truncate (/ w 65536)) 256))
        (a  (mod (truncate (/ w 256)) 256))
        (b  (mod w 256)))
    (cond ((= op  0) (sr m HLI 1))
          ((= op  1) (sr m a b))
          ((= op  2) (sr m a (+ (gr m a) (gr m b))))
          ((= op 11) (print (str-join (list "> " (num-to-str (gr m a))))))
          ((= op 14) (spush m (gr m a)))
          ((= op 15) (sr m a (spop m)))
          (t nil))))

(define (run m budget)
  (sr m SP 254)
  (labels (((cycle n)
      (cond ((= (gr m HLI) 1) n) ((= n budget) n)
            (t (let ((p (gr m PCI))) (let ((w (rd m p))) (sr m PCI (+ p 1)) (exec m w) (cycle (+ n 1))))))))
    (cycle 0)))

(define (load-prog m ws)
  (labels (((go w a) (cond ((null w) a) (t (progn (wr m a (car w)) (go (cdr w) (+ a 1)))))))
    (go ws 0)))

; Forth compiler: returns machine words. p means print.
(define (forth ts)
  (append (fc ts nil) (list (iw 0 0 0))))
(define (fc ts out)
  (cond ((null ts) out)
        ((numberp (car ts)) (fc (cdr ts) (append out (list (iw 1 0 (car ts)) (iw 14 0 0)))))
        ((eq (car ts) '+)    (fc (cdr ts) (append out (list (iw 15 1 0) (iw 15 0 0) (iw 2 0 1) (iw 14 0 0)))))
        ((eq (car ts) 'p)    (fc (cdr ts) (append out (list (iw 15 0 0) (iw 11 0 0)))))
        (t (fc (cdr ts) out))))

(define (str-join parts)
  (cond ((null parts) "") ((null (cdr parts)) (car parts)) (t (cat (car parts) (str-join (cdr parts))))))

; === DEMO ===
(print "=== SISP Forth VM ===")
(print "3 + 4 =")
(define cpu (mk))
(load-prog cpu (forth (list 3 4 '+ 'p)))
(sr cpu PCI 0)
(run cpu 30)
(print (str-join (list "CPU R0 = " (num-to-str (gr cpu 0)))))
(print "=== Done ===")
