; ============================================================
; BOOTLOADER — LOAD AND START THE KERNEL
;
; Reads the OS kernel image from DISK (the OS's flat file store),
; loads it into RAM at address 0, initializes the CPU, and jumps
; to the kernel entry point.
;
; DISK format: list of (name . value) pairs.
; The kernel is stored under the name 'kernel-img as a list of
; assembled machine words.
; ============================================================

(define WORD 16777216) (define MEM 256) (define NREG 10) (define PCI 8) (define HLI 9)

(define (rd  m a) (truncate (mat-ref (car m) 0 a)))
(define (wr  m a v) (mat-set (car m) 0 a (mod v WORD)))
(define (rreg m r) (truncate (mat-ref (car (cdr m)) 0 r)))
(define (wreg m r v) (mat-set (car (cdr m)) 0 r (mod v WORD)))
(define (get-pc m) (rreg m PCI))
(define (set-pc m v) (wreg m PCI v))
(define (halt-cpu m) (wreg m HLI 1))

; Load a list of words into RAM starting at address 0.
(define (load-words m words)
  (labels (((go w a)
      (cond ((null w) a) (t (progn (wr m a (car w)) (go (cdr w) (+ a 1)))))))
    (go words 0)))

; Look up a file by name in the DISK (assoc list).
(define (disk-read disk name)
  (let ((entry (assoc name disk)))
    (cond ((eq entry nil) nil)
          (t (cdr entry)))))

; Boot the kernel: load image from disk, set PC to entry-point, run.
; Returns the CPU state after boot.
(define (boot disk entry-addr budget)
  (let ((kernel-img (disk-read disk 'kernel-img)))
    (cond ((eq kernel-img nil)
           (print "BOOT: kernel image not found on disk"))
          (t (let ((cpu (list (make-matrix 1 MEM) (make-matrix 1 NREG))))
               (load-words cpu kernel-img)
               (set-pc cpu entry-addr)
               (run-cpu cpu budget)
               cpu)))))

; Simple CPU run loop (minimal, for bootstrapping).
(define (run-cpu m budget)
  (labels (((cycle n)
      (cond ((= (rreg m HLI) 1) n)
            ((= n budget) n)
            (t (let ((p (get-pc m)))
                 (let ((w (rd m p)))
                   (set-pc m (+ p 1))
                   (exec-instr m w)
                   (cycle (+ n 1))))))))
    (cycle 0)))

; Execute one instruction (subset: halt, loadi, add, out, jmp, jz).
(define (exec-instr m w)
  (let ((op (mod (truncate (/ w 65536)) 256))
        (a  (mod (truncate (/ w 256)) 256))
        (b  (mod w 256)))
    (cond ((= op 0) (halt-cpu m))
          ((= op 1) (wreg m a b))
          ((= op 2) (wreg m a (+ (rreg m a) (rreg m b))))
          ((= op 8) (set-pc m a))
          ((= op 9) (cond ((= (rreg m a) 0) (set-pc m b))))
          ((= op 11) (rreg m a))
          (t nil))))

; ---- demo: build a "kernel" that prints 42 and halts ---------
(define demo-kernel
  (list (+ (* 1 65536) (* 0 256) 42)     ; loadi r0 42
        (+ (* 11 65536) (* 0 256) 0)     ; out r0
        0))                               ; halt

(define demo-disk (list (cons 'kernel-img demo-kernel)))

(define booted-cpu (boot demo-disk 0 100))
(rreg booted-cpu 0)   ; => 42
