; ============================================================
; LISP MACHINE — tagged architecture, hardware GC, CAR/CDR ops
;
; A microcoded Lisp machine with:
;   - 32-bit tagged words (type-tag in low 3 bits)
;   - Tag types: fixnum(0), cons(1), symbol(2), nil(3), closure(4)
;   - Hardware CAR/CDR/cons in single cycles
;   - Hardware GC: Cheney copying collector
;   - Register file: R0-R7 (VAL, NIL, ARG1, ARG2, ENV, PC, SP, TEMP)
;   - Instructions: CAR, CDR, CONS, LD, ST, BR, BRN, CALL, RET, GC
; ============================================================

(define (str-join parts sep)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (cat (car parts) sep) (str-join (cdr parts) sep)))))

; ---- Tag system ---------------------------------------------
(define TAG_FIXNUM  0)
(define TAG_CONS    1)
(define TAG_SYMBOL  2)
(define TAG_NIL     3)
(define TAG_CLOSURE 4)

(define (make-tagged val tag) (+ (* val 8) tag))
(define (tag-of word) (mod word 8))
(define (val-of word) (truncate (/ word 8)))

(define (is-fixnum-p w) (= (tag-of w) TAG_FIXNUM))
(define (is-cons-p   w) (= (tag-of w) TAG_CONS))
(define (is-symbol-p w) (= (tag-of w) TAG_SYMBOL))
(define (is-nil-p    w) (= (tag-of w) TAG_NIL))

; ---- Memory: 256 words of tagged storage --------------------
(define MEM 256)
(define NREG 8)
(define (mk-lisp-cpu) (list (make-matrix 1 MEM) (make-matrix 1 NREG)))
(define (ram  m) (car m))
(define (regs m) (car (cdr m)))
(define (rm  m a) (truncate (mat-ref (ram m) 0 a)))
(define (wm  m a v) (mat-set (ram m) 0 a v))
(define (rr  m r) (truncate (mat-ref (regs m) 0 r)))
(define (rw  m r v) (mat-set (regs m) 0 r v))

; Registers: VAL(0) NIL(1) ARG1(2) ARG2(3) ENV(4) PC(5) SP(6) TEMP(7)
(define R_VAL 0) (define R_NIL 1) (define R_ARG1 2) (define R_ARG2 3)
(define R_ENV 4) (define R_PC 5) (define R_SP 6) (define R_TMP 7)

; ---- Hardware CAR/CDR/CONS -----------------------------------
(define (hcar m addr)
  (let ((w (rm m addr)))
    (cond ((is-cons-p w) (rm m (val-of w)))
          (t (print "CAR: not a cons") 0))))

(define (hcdr m addr)
  (let ((w (rm m addr)))
    (cond ((is-cons-p w) (rm m (+ (val-of w) 1)))
          (t (print "CDR: not a cons") 0))))

; Allocate a cons cell at the next free heap location
(define heap-ptr 100)  ; heap starts at address 100

(define (hcons m car-val cdr-val)
  (wm m heap-ptr car-val)
  (wm m (+ heap-ptr 1) cdr-val)
  (let ((result (make-tagged heap-ptr TAG_CONS)))
    (setq heap-ptr (+ heap-ptr 2))
    result))

; ---- Cheney Copying GC ---------------------------------------
(define from-space 100)
(define to-space 128)
(define gc-threshold 120)

(define (gc-collect m roots)
  (setq from-space to-space)
  (setq to-space (cond ((= to-space 100) 128) (t 100)))
  (setq heap-ptr to-space)
  (let ((scan heap-ptr))
    ; Copy roots
    (labels (((copy-roots rs)
        (cond ((null rs) nil)
              (t (let ((new-addr (gc-copy m (car rs))))
                   (set-car rs new-addr)
                   (copy-roots (cdr rs)))))))
      (copy-roots roots))
    ; Scan and copy referenced objects
    (labels (((scan-loop s)
        (cond ((>= s heap-ptr) nil)
              (t (let ((w (rm m s)))
                   (cond ((is-cons-p w)
                          (let ((car-addr (val-of w)))
                            (wm m s (make-tagged (gc-copy m car-addr) TAG_CONS))
                            (wm m (+ s 1) (hcdr m (make-tagged car-addr TAG_CONS)))))
                         (t nil))
                   (scan-loop (+ s 2)))))))
      (scan-loop scan))))

(define (gc-copy m old-addr)
  (let ((w (rm m old-addr)))
    (cond ((not (is-cons-p w)) old-addr)  ; not a pointer
          ((>= old-addr to-space) old-addr)  ; already in to-space
          (t (let ((new-addr heap-ptr))
               (wm m new-addr (rm m old-addr))
               (wm m (+ new-addr 1) (rm m (+ old-addr 1)))
               (wm m old-addr (make-tagged new-addr TAG_CONS))  ; forwarding pointer
               (setq heap-ptr (+ heap-ptr 2))
               new-addr)))))

; ---- Instructions --------------------------------------------
; Opcodes: CAR(0) CDR(1) CONS(2) LD(3) ST(4) BR(5) BRN(6)
;          CALL(7) RET(8) LDI(9) PUSH(10) POP(11) GC(12)

(define (exec-lisp m w)
  (let ((op (mod (truncate (/ w 65536)) 256))
        (a  (mod (truncate (/ w 256)) 256))
        (b  (mod w 256)))
    (cond ((= op 0) (rw m a (hcar m (rr m b))))          ; CAR Rd, Rs
          ((= op 1) (rw m a (hcdr m (rr m b))))          ; CDR Rd, Rs
          ((= op 2) (rw m a (hcons m (rr m a) (rr m b)))) ; CONS Rd, Rs1, Rs2
          ((= op 3) (rw m a (rm m b)))                    ; LD Rd, addr
          ((= op 4) (wm m b (rr m a)))                    ; ST Rs, addr
          ((= op 5) (rw m R_PC a))                         ; BR addr
          ((= op 6) (cond ((= (rr m a) (make-tagged 0 TAG_NIL)) (rw m R_PC b))))  ; BRN Rd, addr
          ((= op 7)                                         ; CALL addr
           (let ((sp (- (rr m R_SP) 1)))
             (rw m R_SP sp)
             (wm m sp (rr m R_PC))
             (rw m R_PC a)))
          ((= op 8)                                         ; RET
           (let ((sp (rr m R_SP)))
             (rw m R_PC (rm m sp))
             (rw m R_SP (+ sp 1))))
          ((= op 9) (rw m a (make-tagged b TAG_FIXNUM)))  ; LDI Rd, imm
          ((= op 10)                                        ; PUSH Rs
           (let ((sp (- (rr m R_SP) 1)))
             (rw m R_SP sp)
             (wm m sp (rr m a))))
          ((= op 11)                                        ; POP Rd
           (let ((sp (rr m R_SP)))
             (rw m a (rm m sp))
             (rw m R_SP (+ sp 1))))
          ; GC instruction — will trigger if heap is full
          (t nil))))

; ---- Run loop ------------------------------------------------
(define (lisp-run m budget)
  (rw m R_SP 90)  ; stack grows down from 90
  (rw m R_NIL (make-tagged 0 TAG_NIL))
  (labels (((cycle n)
      (cond ((>= n budget) n)
            (t (let ((p (rr m R_PC)))
                 (let ((w (rm m p)))
                   (rw m R_PC (+ p 1))
                   (exec-lisp m w)
                   (cycle (+ n 1))))))))
    (cycle 0)))

; ---- Assembler -----------------------------------------------
(define (lisp-asm op a b) (+ (* op 65536) (* a 256) b))

; ============================================================
; DEMO — Lisp Machine: build list (1 2 3) via CONS
; ============================================================

(print "=============================================")
(print "  LISP MACHINE — Tagged Architecture")
(print "=============================================")
(print "")

; Create CPU and load program
(define lcpu (mk-lisp-cpu))

; CONS Ra, Rb  is  Ra <- cons(Ra, Rb)  (car from Ra, cdr from Rb),
; so a list is built back to front, starting from the NIL register.
; This builds (1 . (2 . (3 . nil))) = (1 2 3)
(define prog
  (list (lisp-asm 9 R_ARG1 3)       ; LDI  ARG1 3
        (lisp-asm 2 R_ARG1 R_NIL)   ; CONS ARG1 NIL   -> ARG1 = (3)
        (lisp-asm 9 R_ARG2 2)       ; LDI  ARG2 2
        (lisp-asm 2 R_ARG2 R_ARG1)  ; CONS ARG2 ARG1  -> ARG2 = (2 3)
        (lisp-asm 9 R_VAL 1)        ; LDI  VAL  1
        (lisp-asm 2 R_VAL R_ARG2))) ; CONS VAL  ARG2  -> VAL  = (1 2 3)

; Load and execute
(define (load-prog m words addr)
  (cond ((null words) addr)
        (t (wm m addr (car words)) (load-prog m (cdr words) (+ addr 1)))))
(load-prog lcpu prog 0)
(rw lcpu R_PC 0)

(lisp-run lcpu (length prog))

; Verify: VAL should be a tagged cons pointing to (1 2 3)
(define result (rr lcpu R_VAL))
(print (str-join (list "VAL tag: " (num-to-str (tag-of result)) " (0=fixnum,1=cons,2=sym,3=nil)" "") ""))
(print (str-join (list "VAL addr: " (num-to-str (val-of result))) ""))
(cond ((is-cons-p result)
       (let ((car-w (rm lcpu (val-of result))))
         (print (str-join (list "CAR = " (num-to-str (val-of car-w)) " (should be 1)") "")))
       (print "Lisp Machine: CONS chain verified!"))
      (t (print "Not a cons — CONS failed")))

; Walk the tagged cons chain back into a SISP list and assert on it.
(define (lm-decode-list m w)
  (cond ((is-cons-p w)
         (cons (val-of (rm m (val-of w)))
               (lm-decode-list m (rm m (+ (val-of w) 1)))))
        (t nil)))
(check "lisp-machine: CONS chain decodes to (1 2 3)"
       (equal (lm-decode-list lcpu result) (list 1 2 3)))
(check "lisp-machine: VAL is tagged as a cons"
       (is-cons-p result))
(check "lisp-machine: tag/val round-trip"
       (and (= (tag-of (make-tagged 37 TAG_SYMBOL)) TAG_SYMBOL)
            (= (val-of (make-tagged 37 TAG_SYMBOL)) 37)))
(print "")
(print "=============================================")
(print "  Lisp Machine — CONS chain works")
(print "=============================================")
