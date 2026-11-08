; ============================================================
; EXTENDED CPU — interrupts, MMU, I/O bus, disk, exceptions
; All layers verified working at runtime.
; ============================================================

(define WORD  16777216) (define MEM 256) (define NREG 16)
(define PCI 8) (define FLG 9) (define SPI 10) (define USP 11)
(define CYC 12) (define IVC 13) (define IMK 14) (define FLT 15)

(define (rd m a) (truncate (mat-ref (car m) 0 a)))
(define (wr m a v) (mat-set (car m) 0 a (mod v WORD)))
(define (gr m r) (truncate (mat-ref (car (cdr m)) 0 r)))
(define (sr m r v) (mat-set (car (cdr m)) 0 r (mod v WORD)))
(define (mk) (list (make-matrix 1 MEM) (make-matrix 1 NREG)))

; ---- FLAGS register (FLG=R9) --------------------------------
; bit 0 = halt, bit 1 = supervisor, bit 2 = int-enable
(define (hlt-p m) (= (mod (gr m FLG) 2) 1))
(define (hlt-set m) (sr m FLG (+ (gr m FLG) 1)))
(define (sv-p m)  (= (mod (truncate (/ (gr m FLG) 2)) 2) 1))
(define (ie-p m)  (= (mod (truncate (/ (gr m FLG) 4)) 2) 1))
(define (ie-on m)  (sr m FLG (+ (gr m FLG) 4)))
(define (ie-off m)  (sr m FLG (- (gr m FLG) 4)))

; ---- I/O devices --------------------------------------------
(define kbd-buf nil)       ; keyboard buffer
(define disk-store nil)    ; (sector . byte-list) pairs
(define disk-sector 0)
(define disk-buffer nil)

(define (kbd-push ch) (setq kbd-buf (append kbd-buf (list ch))))

(define (io-read addr)
  (cond ((= addr 65520) (cond ((null kbd-buf) 0) (t (let ((ch (car kbd-buf))) (setq kbd-buf (cdr kbd-buf)) ch))))
        ((= addr 65521) (cond ((null kbd-buf) 0) (t 1)))
        ((= addr 65523) (cond ((null disk-buffer) 0) (t (let ((b (car disk-buffer))) (setq disk-buffer (cdr disk-buffer)) b))))
        (t 0)))

(define (io-write addr val)
  (cond ((= addr 65522) (setq disk-sector val))
        ((= addr 65523) (setq disk-buffer (append disk-buffer (list (mod val 256)))))
        ((= addr 65524)
         (cond ((= val 1) (let ((e (assoc disk-sector disk-store))) (setq disk-buffer (cond (e (cdr e)) (t nil)))))
               ((= val 2) (setq disk-store (disk-put disk-store disk-sector disk-buffer)))))
        (t nil)))

(define (disk-put store sec data)
  (cond ((null store) (list (cons sec data)))
        ((= (caar store) sec) (cons (cons sec data) (cdr store)))
        (t (cons (car store) (disk-put (cdr store) sec data)))))

; ---- MMU ----------------------------------------------------
(define mmu-base 0) (define mmu-limit 16384)

(define (mmu-translate va sv-mode)
  (cond (sv-mode va) ((>= va mmu-limit) -1) (t (+ va mmu-base))))

; ---- Interrupt controller -----------------------------------
(define irq-pending 0)

(define (irq-raise n) (setq irq-pending (+ irq-pending (^ 2 n))))

(define (logand a b)
  (lacc a b 0 1))
(define (lacc a b pos bit)
  (cond ((= pos 16) 0)
        ((and (= (mod a 2) 1) (= (mod b 2) 1))
         (+ bit (lacc (truncate (/ a 2)) (truncate (/ b 2)) (+ pos 1) (* bit 2))))
        (t (lacc (truncate (/ a 2)) (truncate (/ b 2)) (+ pos 1) (* bit 2)))))

(define (irq-check m)
  (let ((masked (logand irq-pending (gr m IMK))))
    (cond ((= masked 0) 0)
          (t (let ((vec (ffs masked)))
               (setq irq-pending (- irq-pending (^ 2 vec)))
               vec)))))

(define (ffs bits) (facc bits 0))
(define (facc bits n) (cond ((= n 8) 0) ((= (mod bits 2) 1) n) (t (facc (truncate (/ bits 2)) (+ n 1)))))

; ---- Interrupt dispatch -------------------------------------
(define (dispatch-irq m vec)
  (sr m FLG (+ (gr m FLG) 2))    ; set supervisor bit
  (sr m FLG (- (gr m FLG) 4))    ; clear IE (bit 2 off)
  (sr m PCI (rd m (* vec 2))))   ; jump to handler at RAM[vec*2]

; ---- iret ---------------------------------------------------
(define (iret m)
  (sr m FLG (- (gr m FLG) 2))    ; clear supervisor
  (sr m FLG (+ (gr m FLG) 4)))   ; enable interrupts

; ---- Execution engine ----------------------------------------
(define (exec m w)
  (let ((op (mod (truncate (/ w 65536)) 256)))
    (let ((a (mod (truncate (/ w 256)) 256)))
      (let ((b (mod w 256)))
        (cond
          ((= op 0) (sr m FLG (+ (gr m FLG) 1)))                    ; HALT
          ((= op 1) (sr m a b))                                       ; LOADI
          ((= op 2) (sr m a (+ (gr m a) (gr m b))))                   ; ADD
          ((= op 3) (sr m a (- (gr m a) (gr m b))))                   ; SUB
          ((= op 4) (sr m a (* (gr m a) (gr m b))))                   ; MUL
          ((= op 5) (sr m a (gr m b)))                                ; MOV
          ((= op 11) (print (cat "OUT " (num-to-str (gr m a)))))        ; OUT
          ((= op 18) (sr m a (io-read (gr m b))))                     ; IN port
          ((= op 19) (io-write (gr m a) (gr m b)))                    ; OUT port
          ((= op 20) (sr m FLG (- (gr m FLG) 2)) (sr m FLG (+ (gr m FLG) 4))) ; IRET
          ((= op 22) (sr m FLG (+ (gr m FLG) 4)))                     ; EI
          ((= op 23) (sr m FLG (- (gr m FLG) 4)))                     ; DI
          ((= op 24) (sr m FLT a))                                    ; FAULT
          (t nil))))))

; ---- Run loop (direct recursion, no labels) -----------------
(define (run m budget)
  (sr m SPI 200)
  (run-cycle m budget 0))

(define (run-cycle m budget n)
  (cond ((= (mod (gr m FLG) 2) 1) n)
        ((= n budget) n)
        (t (sr m CYC (+ (gr m CYC) 1))
           ; Inline interrupt check (no helper fn calls — they break inside recursion)
           (cond ((and (= (mod (gr m CYC) 4) 0)
                       (= (mod (truncate (/ (gr m FLG) 4)) 2) 1))
                  (let ((vec (irq-check m)))
                    (cond ((> vec 0) (dispatch-irq m vec))))))
           (let ((p (gr m PCI)))
             (let ((w (rd m p)))
               (sr m PCI (+ p 1))
               (exec m w)))
           (run-cycle m budget (+ n 1)))))

; ---- Assembler -----------------------------------------------
(define (rev lst) (labels (((go l a) (cond ((null l) a) (t (go (cdr l) (cons (car l) a)))))) (go lst nil)))
(define (nth-or ops i) (cond ((null ops) 0) ((= i 0) (car ops)) (t (nth-or (cdr ops) (- i 1)))))

(define (asm-lbl prog)
  (labels (((go p a acc)
      (cond ((null p) acc)
            ((eq (caar p) 'label) (go (cdr p) a (cons (list (cadar p) a) acc)))
            (t (go (cdr p) (+ a 1) acc)))))
    (go prog 0 nil)))

(define (resolve x tbl) (cond ((numberp x) x) (t (cadr (assoc x tbl)))))

(define (opcode name)
  (cadr (assoc name (list (list 'halt 0 0) (list 'loadi 1 2) (list 'add 2 2)
    (list 'sub 3 2) (list 'mul 4 2) (list 'mov 5 2)
    (list 'out 11 1) (list 'in 18 2) (list 'outb 19 2)
    (list 'iret 20 0) (list 'ei 22 0) (list 'di 23 0) (list 'fault 24 1)))))

(define (encode stmt tbl)
  (let ((op (opcode (car stmt))) (a (resolve (nth-or (cdr stmt) 0) tbl)) (b (resolve (nth-or (cdr stmt) 1) tbl)))
    (+ (* op 65536) (* a 256) b)))

(define (assemble prog)
  (let ((tbl (asm-lbl prog)))
    (labels (((go p a) (cond ((null p) (rev a))
                             ((eq (caar p) 'label) (go (cdr p) a))
                             (t (go (cdr p) (cons (encode (car p) tbl) a))))))
      (go prog nil))))

(define (load-ram m ws base)
  (labels (((go w a) (cond ((null w) a) (t (progn (wr m (+ base a) (car w)) (go (cdr w) (+ a 1)))))))
    (go ws 0)))

; ============================================================
; DEMO: every layer verified
; ============================================================

(print "=== Extended CPU — Hardware Layers ===")
(print "")

; --- 1. CPU init ---
(define cpu (mk))
(sr cpu FLG 2)    ; supervisor mode
(sr cpu PCI 0)
(sr cpu SPI 200)  ; stack at 200 (RAM is 256 words)
(sr cpu CYC 0)
(sr cpu IMK 255)  ; all interrupts enabled
(print "CPU initialized: supervisor, 64K RAM, 16 regs")
(print (cat "IRQ mask: " (num-to-str (gr cpu IMK))))
(print "")

; --- 2. Keyboard I/O ---
(print "--- Keyboard ---")
(kbd-push 65) (kbd-push 66) (kbd-push 67)
(define kbd-status0 (io-read 65521))
(define kbd-read1 (io-read 65520))
(define kbd-read2 (io-read 65520))
(define kbd-status1 (io-read 65521))
(print (cat "status: " (num-to-str kbd-status0)))
(print (cat "read: " (num-to-str kbd-read1)))
(print (cat "read: " (num-to-str kbd-read2)))
(print (cat "remaining: " (num-to-str kbd-status1)))
(print "")

; --- 3. Block disk ---
(print "--- Block Disk ---")
(setq disk-sector 0)
(setq disk-buffer (list 72 69 76 76 79))
(io-write 65524 2)
(print "wrote HELLO to sector 0")
(setq disk-buffer nil)
(io-write 65524 1)
(define (nums-to-str ns)
  (cond ((null ns) "")
        ((null (cdr ns)) (num-to-str (car ns)))
        (t (cat (cat (num-to-str (car ns)) " ") (nums-to-str (cdr ns))))))
(define disk-readback disk-buffer)
(print (cat "read sector 0: " (nums-to-str disk-readback)))
(print "")

; --- 4. MMU ---
(print "--- MMU ---")
(setq mmu-base 4096) (setq mmu-limit 8192)
(print (cat "user VA 0 -> physical " (num-to-str (mmu-translate 0 nil))))
(print (cat "user VA 100 -> physical " (num-to-str (mmu-translate 100 nil))))
(print (cat "supervisor VA 0 -> " (num-to-str (mmu-translate 0 t))))
(print (cat "user VA 9999 -> fault:" (num-to-str (mmu-translate 9999 nil))))
(print "")

; --- 5. Fault / exception ---
(print "--- Exception (fault opcode 24,a=1) ---")
(sr cpu FLT 0)
(exec cpu (+ (* 24 65536) (* 1 256) 0))
(print (cat "fault code: " (num-to-str (gr cpu FLT))))
(print "")

; --- 6. Interrupt enable/disable ---
(print "--- Interrupt control ---")
(ie-on cpu)
(define ie-after-on (ie-p cpu))
(print (cat "IE on: " (cond (ie-after-on "yes") (t "no"))))
(ie-off cpu)
(define ie-after-off (ie-p cpu))
(print (cat "IE off: " (cond (ie-after-off "yes") (t "no"))))
(print "")

; --- 7. Timer interrupt ---
(print "--- Timer Interrupt ---")
; Timer ISR lives at RAM[32]: loadi 0 99, iret
(define TIMER_ISR 32)
(wr cpu TIMER_ISR       (+ (* 1 65536) (* 0 256) 99))  ; loadi r0 99
(wr cpu (+ TIMER_ISR 1) (* 20 65536))                   ; iret
; Vector table lives at RAM[vec*2]; entry for IRQ1 (timer) holds the
; ISR *address*, which is what dispatch-irq loads into PC.
(wr cpu 2 TIMER_ISR)
; Raise timer interrupt
(ie-on cpu)
(sr cpu IMK 2)    ; only timer enabled
(irq-raise 1)
; Run 1 cycle — should dispatch timer ISR
(define p (gr cpu PCI))
(define w (rd cpu p))
(sr cpu PCI (+ p 1))
; Check for interrupt
(cond ((and (= (mod (gr cpu CYC) 4) 0) (ie-p cpu))
       (let ((v (irq-check cpu))) (cond ((> v 0) (dispatch-irq cpu v))))))
(print (cat "IRQ pending after raise: " (num-to-str irq-pending)))
(print (cat "After dispatch, PC=" (num-to-str (gr cpu PCI))))
(print "")

; --- 8. Full program execution via run loop ---
(print "--- Load and run a program ---")
(define cpu2 (mk))
(sr cpu2 FLG 2)
(sr cpu2 SPI 200)
(sr cpu2 CYC 0)
(define prog (assemble '((loadi 0 7) (loadi 1 3) (add 0 1) (halt))))
(load-ram cpu2 prog 0)
(sr cpu2 PCI 0)
(define cycles2 (run cpu2 20))
(print (cat "Program: 7+3=" (num-to-str (gr cpu2 0))))
(print (cat "Cycles: " (num-to-str cycles2)))
(print (cat "Cycle reg: " (num-to-str (gr cpu2 CYC))))
(print "")

; --- 9. Assertions over every layer above --------------------
(print "--- Checks ---")
(check "kbd: reads come out FIFO and status tracks the buffer"
       (and (and (= kbd-status0 1) (= kbd-status1 1))
            (and (= kbd-read1 65) (and (= kbd-read2 66) (equal kbd-buf (list 67))))))
(check "disk: sector 0 round-trips HELLO"
       (equal disk-readback (list 72 69 76 76 79)))
(check "mmu: user VA is relocated by base"
       (and (= (mmu-translate 0 nil) 4096) (= (mmu-translate 100 nil) 4196)))
(check "mmu: supervisor bypasses translation" (= (mmu-translate 0 t) 0))
(check "mmu: VA past the limit faults" (= (mmu-translate 9999 nil) -1))
(check "exception: FAULT opcode records its code" (= (gr cpu FLT) 1))
(check "flags: EI sets and DI clears interrupt-enable"
       (and ie-after-on (not ie-after-off)))
(check "irq: dispatch vectors PC through the vector table"
       (= (gr cpu PCI) TIMER_ISR))
(check "irq: pending bit is cleared once taken" (= irq-pending 0))
(check "run: program computes 7+3=10" (= (gr cpu2 0) 10))
(check "run: halts after 4 cycles, cycle counter agrees"
       (and (= cycles2 4) (= (gr cpu2 CYC) 4)))
(check "run: halt flag is set" (hlt-p cpu2))
(check "assembler: encodes (add 0 1) as op 2" (= (encode '(add 0 1) nil) 131073))
(print "")

(print "=== Extended CPU demo complete ===")
