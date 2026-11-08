; ============================================================
; SISP MACHINE V2.1 — Complete OS-ready CPU
;
; 24-bit word, 4096W RAM, 16 registers, MMU (base/limit),
; vectored interrupts with IRET, timer + keyboard IRQs,
; line-buffered character console, 64-sector disk, faults,
; condition codes, shifts and logic ops, hypercalls (SYS).
;
; Design:
;   * every instruction is a flat opcode (0..34) with two 8-bit
;     operand fields;
;   * IRET: interrupt handlers return to the interrupted code;
;   * SYS n: hypercall — stops the CPU with FLT = 256+n so the
;     host kernel/emulator takes over (a "VM exit");
;   * faults (illegal op, div0, MMU, privilege) set FLT and stop
;     the fetch/execute loop;
;   * CMP sets Z/L flags; JL/JG/JE/JNE branch on them;
;   * SHL/SHR/AND/OR/XOR/NOT, PUSH/POP, CALL/RET, JMPR (long
;     jump through a register);
;   * user mode: PC and data addresses are virtual — all control
;     flow targets and loads/stores go through the MMU, and
;     fetching outside the window faults;
;   * console TX is character-true with a line buffer: writing
;     bytes to port 3840 prints a line when '\n' arrives;
;   * disk has geometry ports and replaces sectors on rewrite.
;
; ISA (word = op*65536 + a*256 + b):
;   0  halt            18 ei               32 iret
;   1  loadi Ra imm8   19 di               33 push Ra
;   2  load  Ra [Rb]   20 tas  Ra          34 pop  Ra
;   3  store [Ra] Rb   21 cmp  Ra Rb
;   4  add   Ra Rb     22 jl   t8
;   5  sub   Ra Rb     23 jg   t8
;   6  mul   Ra Rb     24 je   t8
;   7  div   Ra Rb     25 jne  t8
;   8  mov   Ra Rb     26 shl  Ra Rb
;   9  jmp   t8        27 shr  Ra Rb
;  10  jz    Ra t8     28 and  Ra Rb
;  11  jnz   Ra t8     29 or   Ra Rb
;  12  jmpr  Ra        30 xor  Ra Rb
;  13  sys   imm8      31 not  Ra
;  14  call  t8
;  15  ret
;  16  outb  Ra Rb     port[Ra] <- Rb
;  17  inb   Ra Rb     Ra <- port[Rb]
;
; Flags (FLG): bit0 halt, bit1 supervisor, bit2 int-enable,
;              bit3 Z, bit4 L (set by CMP).
; Faults (FLT): 1 illegal-op, 2 div0, 3 MMU, 4 privilege,
;              >=256: SYS hypercall number (FLT-256).
; ============================================================

(define WORD 16777216) (define MEM 4096) (define NREG 16)
(define PCI 8) (define FLG 9) (define SPI 10) (define USP 11)
(define CYC 12) (define IVEC 13) (define IMSK 14) (define FLT 15)

(define (rr m r) (truncate (mat-ref (car (cdr m)) 0 r)))
(define (rw m r v) (mat-set (car (cdr m)) 0 r (mod v WORD)))
(define (rd m a) (truncate (mat-ref (car m) 0 a)))
(define (wr m a v) (mat-set (car m) 0 a (mod v WORD)))
(define (mk-cpu) (list (make-matrix 1 MEM) (make-matrix 1 NREG)))
(define (iw op a b) (+ (* op 65536) (* a 256) b))
(define (hi w) (mod (truncate (/ w 65536)) 256))
(define (mid w) (mod (truncate (/ w 256)) 256))
(define (lo w) (mod w 256))

; ---- flags ---------------------------------------------------
(define (bit w n) (mod (truncate (/ w (^ 2 n))) 2))
(define (halt-p m) (= (bit (rr m FLG) 0) 1))
(define (halt m) (rw m FLG (+ (rr m FLG) 1)))
(define (sv-p m) (= (bit (rr m FLG) 1) 1))
(define (ie-p m) (= (bit (rr m FLG) 2) 1))
(define (ie-on m) (cond ((not (ie-p m)) (rw m FLG (+ (rr m FLG) 4)))))
(define (ie-off m) (cond ((ie-p m) (rw m FLG (- (rr m FLG) 4)))))
(define (z-flag m) (bit (rr m FLG) 3))
(define (l-flag m) (bit (rr m FLG) 4))
; CMP: set Z=(a=b), L=(a<b), preserving bits 0-2.
(define (set-flags m a b)
  (rw m FLG (+ (mod (rr m FLG) 8)
               (* (cond ((= a b) 1) (t 0)) 8)
               (* (cond ((< a b) 1) (t 0)) 16))))
(define (fault m n) (rw m FLT n))

; ---- MMU -----------------------------------------------------
(define mmu-base 0) (define mmu-limit 4096)
; virtual -> (physical fault?)
(define (mmu-trans va)
  (cond ((>= va mmu-limit) (list 0 t))
        (t (list (+ va mmu-base) nil))))
; Data accesses: translated in user mode, physical in supervisor.
(define (mem-read m va)
  (cond ((sv-p m) (rd m va))
        (t (let ((tr (mmu-trans va)))
             (cond ((cadr tr) (fault m 3) 0)
                   (t (rd m (car tr))))))))
(define (mem-write m va v)
  (cond ((sv-p m) (wr m va v))
        (t (let ((tr (mmu-trans va)))
             (cond ((cadr tr) (fault m 3))
                   (t (wr m (car tr) v)))))))
; Control-flow targets are virtual in user mode.
(define (jaddr m tgt)
  (cond ((sv-p m) tgt)
        ((>= tgt mmu-limit) (fault m 3) (rr m PCI))
        (t (+ mmu-base tgt))))
; Fetch: user PC must stay inside the window.
(define (fetch-word m p)
  (cond ((sv-p m) (rd m p))
        ((or (< p mmu-base) (>= p (+ mmu-base mmu-limit))) (fault m 3) 0)
        (t (rd m p))))

; ---- stacks --------------------------------------------------
; Kernel stack uses SPI (always physical); a user-mode PUSH/POP or
; CALL/RET uses USP and goes through the MMU.
(define (k-push m v) (let ((s (- (rr m SPI) 1))) (rw m SPI s) (wr m s v)))
(define (k-pop m) (let ((s (rr m SPI))) (let ((v (rd m s))) (rw m SPI (+ s 1)) v)))
(define (sp-reg m) (cond ((sv-p m) SPI) (t USP)))
(define (m-push m v)
  (let ((r (sp-reg m))) (let ((s (- (rr m r) 1))) (rw m r s) (mem-write m s v))))
(define (m-pop m)
  (let ((r (sp-reg m))) (let ((s (rr m r))) (let ((v (mem-read m s))) (rw m r (+ s 1)) v))))

; ---- I/O devices ----------------------------------------------
; Console: 3840 write = TX byte (line-buffered, prints on '\n'),
;          3841 write = print number as a line.
; Keyboard: 3840 read = RX byte, 3841 read = RX status.
; Disk:    3842 = sector select, 3843 = command (1 read / 2 write),
;          3844 = data (read pops a byte, write appends a byte),
;          3845 read = sector count, 3846 read = sector size.
(define con-in nil) (define con-line nil)
; When *con-capture-on* is set, console output is appended to the
; *con-capture* char-code list instead of being printed (OS checks).
(define *con-capture-on* nil) (define *con-capture* nil)
(define (kbd-push ch) (setq con-in (append con-in (list ch))) (irq-raise 2))
(define (con-emit code)
  (cond (*con-capture-on* (setq *con-capture* (append *con-capture* (list (mod code 256)))))
        ((= code 10) (print (list-to-str con-line)) (setq con-line nil))
        (t (setq con-line (append con-line (list (code-char (mod code 256))))))))
(define (con-print-num v)
  (labels (((go cs) (cond ((null cs) (con-emit 10)) (t (con-emit (char-code (car cs))) (go (cdr cs))))))
    (go (str-to-list (num-to-str v)))))

(define DSK-NSEC 64) (define DSK-SSZ 256)
(define dsk-store nil) (define dsk-sector 0) (define dsk-buf nil)
(define (disk-put store sec data)
  (cond ((null store) (list (cons sec data)))
        ((= (caar store) sec) (cons (cons sec data) (cdr store)))
        (t (cons (car store) (disk-put (cdr store) sec data)))))
(define (pad-to bs n)
  (cond ((= (length bs) n) bs)
        ((> (length bs) n) (pad-to (reverse (cdr (reverse bs))) n))
        (t (pad-to (append bs (list 0)) n))))
(define (disk-read-sec sec)
  (let ((e (assoc sec dsk-store)))
    (cond (e (cdr e)) (t (pad-to nil DSK-SSZ)))))

(define (io-rd port)
  (cond ((= port 3840) (cond ((null con-in) 0)
                             (t (let ((ch (car con-in))) (setq con-in (cdr con-in)) ch))))
        ((= port 3841) (cond ((null con-in) 0) (t 1)))
        ((= port 3844) (cond ((null dsk-buf) 0)
                             (t (let ((b (car dsk-buf))) (setq dsk-buf (cdr dsk-buf)) b))))
        ((= port 3845) DSK-NSEC)
        ((= port 3846) DSK-SSZ)
        (t 0)))
(define (io-wr port val)
  (cond ((= port 3840) (con-emit val))
        ((= port 3841) (con-print-num val))
        ((= port 3842) (setq dsk-sector (mod val DSK-NSEC)))
        ((= port 3843)
         (cond ((= val 1) (setq dsk-buf (disk-read-sec dsk-sector)))
               ((= val 2) (setq dsk-store (disk-put dsk-store dsk-sector
                                                    (pad-to dsk-buf DSK-SSZ))))))
        ((= port 3844) (setq dsk-buf (append dsk-buf (list (mod val 256)))))
        (t nil)))

; ---- interrupt controller -------------------------------------
; IRQ 1 = timer, IRQ 2 = keyboard.  Dispatch pushes PC+FLG on the
; kernel stack and vectors through IVEC slots (mem[vec*2]).
(define irq-pend 0) (define timr-intv 0)
; Pending bits must be OR-ed in, not added: raising the same IRQ
; twice before it is serviced would otherwise carry into garbage
; vector numbers.
(define (irq-raise n) (setq irq-pend (logor irq-pend (^ 2 n))))
(define (logand a b) (la a b 0 1))
(define (la a b p bt)
  (cond ((= p 25) 0)
        ((and (= (mod a 2) 1) (= (mod b 2) 1))
         (+ bt (la (truncate (/ a 2)) (truncate (/ b 2)) (+ p 1) (* bt 2))))
        (t (la (truncate (/ a 2)) (truncate (/ b 2)) (+ p 1) (* bt 2)))))
(define (logor a b) (lo2 a b 0 1))
(define (lo2 a b p bt)
  (cond ((= p 25) 0)
        ((or (= (mod a 2) 1) (= (mod b 2) 1))
         (+ bt (lo2 (truncate (/ a 2)) (truncate (/ b 2)) (+ p 1) (* bt 2))))
        (t (lo2 (truncate (/ a 2)) (truncate (/ b 2)) (+ p 1) (* bt 2)))))
(define (logxor a b) (lx a b 0 1))
(define (lx a b p bt)
  (cond ((= p 25) 0)
        ((not (= (mod a 2) (mod b 2)))
         (+ bt (lx (truncate (/ a 2)) (truncate (/ b 2)) (+ p 1) (* bt 2))))
        (t (lx (truncate (/ a 2)) (truncate (/ b 2)) (+ p 1) (* bt 2)))))
(define (ffs bits) (fa bits 0))
(define (fa bits n) (cond ((= n 16) 0) ((= (mod bits 2) 1) n) (t (fa (truncate (/ bits 2)) (+ n 1)))))
(define (irq-check m)
  (let ((masked (logand irq-pend (rr m IMSK))))
    (cond ((= masked 0) 0)
          (t (let ((v (ffs masked))) (setq irq-pend (- irq-pend (^ 2 v))) v)))))
(define (irq-dispatch m vec)
  (k-push m (rr m PCI)) (k-push m (rr m FLG))
  (rw m FLG 2) (rw m PCI (rd m (* vec 2))))

; ---- execute one instruction ----------------------------------
(define (exec m w)
  (let ((op (hi w)) (a (mid w)) (b (lo w)))
    (cond ((= op 0) (halt m))
          ((= op 1) (rw m a b))
          ((= op 2) (rw m a (mem-read m (rr m b))))
          ((= op 3) (mem-write m (rr m a) (rr m b)))
          ((= op 4) (rw m a (+ (rr m a) (rr m b))))
          ((= op 5) (rw m a (- (rr m a) (rr m b))))
          ((= op 6) (rw m a (* (rr m a) (rr m b))))
          ((= op 7) (let ((d (rr m b)))
                      (cond ((= d 0) (fault m 2))
                            (t (rw m a (truncate (/ (rr m a) d)))))))
          ((= op 8) (rw m a (rr m b)))
          ((= op 9) (rw m PCI (jaddr m a)))
          ((= op 10) (cond ((= (rr m a) 0) (rw m PCI (jaddr m b)))))
          ((= op 11) (cond ((not (= (rr m a) 0)) (rw m PCI (jaddr m b)))))
          ((= op 12) (rw m PCI (jaddr m (rr m a))))
          ((= op 13) (fault m (+ 256 a)))
          ((= op 14) (m-push m (rr m PCI)) (rw m PCI (jaddr m a)))
          ((= op 15) (rw m PCI (m-pop m)))
          ((= op 16) (io-wr (rr m a) (rr m b)))
          ((= op 17) (rw m a (io-rd (rr m b))))
          ((= op 18) (cond ((sv-p m) (ie-on m)) (t (fault m 4))))
          ((= op 19) (cond ((sv-p m) (ie-off m)) (t (fault m 4))))
          ((= op 20) (let ((addr (rr m a)))
                       (let ((old (mem-read m addr)))
                         (mem-write m addr 1) (rw m a old))))
          ((= op 21) (set-flags m (rr m a) (rr m b)))
          ((= op 22) (cond ((= (l-flag m) 1) (rw m PCI (jaddr m a)))))
          ((= op 23) (cond ((and (= (l-flag m) 0) (= (z-flag m) 0))
                            (rw m PCI (jaddr m a)))))
          ((= op 24) (cond ((= (z-flag m) 1) (rw m PCI (jaddr m a)))))
          ((= op 25) (cond ((= (z-flag m) 0) (rw m PCI (jaddr m a)))))
          ((= op 26) (rw m a (* (rr m a) (^ 2 (rr m b)))))
          ((= op 27) (rw m a (truncate (/ (rr m a) (^ 2 (rr m b))))))
          ((= op 28) (rw m a (logand (rr m a) (rr m b))))
          ((= op 29) (rw m a (logor (rr m a) (rr m b))))
          ((= op 30) (rw m a (logxor (rr m a) (rr m b))))
          ((= op 31) (rw m a (- (- WORD 1) (rr m a))))
          ((= op 32) (rw m FLG (k-pop m)) (rw m PCI (k-pop m)))
          ((= op 33) (m-push m (rr m a)))
          ((= op 34) (rw m a (m-pop m)))
          (t (fault m 1)))))

; ---- run loop -------------------------------------------------
; Stops on HALT, on any fault (including SYS hypercalls), or when
; the budget runs out.  Returns the number of cycles executed.
(define (run m budget)
  (rw m SPI 2048) (rw m FLG 2) (rw m PCI (rd m 0))
  (run1 m budget 0))
(define (run1 m budget n)
  (cond ((halt-p m) n)
        ((> (rr m FLT) 0) n)
        ((= n budget) n)
        (t (rw m CYC (+ (rr m CYC) 1))
           ; timer: raise only when serviceable (IE on, not already
           ; pending) — an IRQ stormier than its handler is livelock
           (cond ((and (> timr-intv 0) (= (mod (rr m CYC) timr-intv) 0)
                       (ie-p m) (= (logand irq-pend 2) 0))
                  (irq-raise 1)))
           (cond ((ie-p m)
                  (let ((v (irq-check m))) (cond ((> v 0) (irq-dispatch m v))))))
           (cond ((= (rr m FLT) 0)
                  (let ((p (rr m PCI)))
                    (rw m PCI (+ p 1))
                    (let ((w (fetch-word m p)))
                      ; a faulting fetch must not execute the phantom
                      ; word 0 (which decodes as HALT)
                      (cond ((= (rr m FLT) 0) (exec m w)))))))
           (run1 m budget (+ n 1)))))

; ============================================================
; V2 ASSEMBLER — two-pass, labels, pseudo-ops
;
; Program = list of statements:
;   (mnemonic a b)   instruction; operands are numbers or labels
;   (label name)     marks the address of the next statement
;   (word v)         raw 24-bit data word
;   (db "str")       one word per character code + terminating 0
;   (loadw Ra v)     pseudo: build a 24-bit constant in Ra
;                    (clobbers R7); minimal expansion for numeric
;                    v, always 9 words when v is a label
; (assemble prog)     -> list of words, org 0
; (assemble-at prog org) -> labels relative to org
; ============================================================
(define ISA2
  (list (list 'halt 0) (list 'loadi 1) (list 'load 2) (list 'store 3)
        (list 'add 4) (list 'sub 5) (list 'mul 6) (list 'div 7)
        (list 'mov 8) (list 'jmp 9) (list 'jz 10) (list 'jnz 11)
        (list 'jmpr 12) (list 'sys 13) (list 'call 14) (list 'ret 15)
        (list 'outb 16) (list 'inb 17) (list 'ei 18) (list 'di 19)
        (list 'tas 20) (list 'cmp 21) (list 'jl 22) (list 'jg 23)
        (list 'je 24) (list 'jne 25) (list 'shl 26) (list 'shr 27)
        (list 'and 28) (list 'or 29) (list 'xor 30) (list 'not 31)
        (list 'iret 32) (list 'push 33) (list 'pop 34)))

(define (nth-or ops i)
  (cond ((null ops) 0) ((= i 0) (car ops)) (t (nth-or (cdr ops) (- i 1)))))

; Expand one loadw into primitive statements (R7 = scratch).
(define (loadw-expand r v)
  (cond ((numberp v)
         (cond ((< v 256) (list (list 'loadi r v)))
               ((< v 65536)
                (list (list 'loadi r (truncate (/ v 256)))
                      (list 'loadi 7 8) (list 'shl r 7)
                      (list 'loadi 7 (mod v 256)) (list 'add r 7)))
               (t (list (list 'loadi r (truncate (/ v 65536)))
                        (list 'loadi 7 8) (list 'shl r 7)
                        (list 'loadi 7 (mod (truncate (/ v 256)) 256)) (list 'add r 7)
                        (list 'loadi 7 8) (list 'shl r 7)
                        (list 'loadi 7 (mod v 256)) (list 'add r 7)))))
        (t (list (list 'loadi r (list 'hi24 v))
                 (list 'loadi 7 8) (list 'shl r 7)
                 (list 'loadi 7 (list 'mid24 v)) (list 'add r 7)
                 (list 'loadi 7 8) (list 'shl r 7)
                 (list 'loadi 7 (list 'lo24 v)) (list 'add r 7)))))

; Pseudo-op expansion; labels stay in place.
;   db  -> string chars + NUL terminator   (filenames, C strings)
;   dbs -> string chars only, no terminator (message bodies; a raw
;          char word like (word 10) can follow for a newline)
(define (asm-expand prog)
  (cond ((null prog) nil)
        ((eq (caar prog) 'loadw)
         (append (loadw-expand (cadr (car prog)) (caddr (car prog)))
                 (asm-expand (cdr prog))))
        ((eq (caar prog) 'db)
         (append (append (map (lambda (ch) (list 'word (char-code ch)))
                              (str-to-list (cadr (car prog))))
                         (list '(word 0)))
                 (asm-expand (cdr prog))))
        ((eq (caar prog) 'dbs)
         (append (map (lambda (ch) (list 'word (char-code ch)))
                      (str-to-list (cadr (car prog))))
                 (asm-expand (cdr prog))))
        (t (cons (car prog) (asm-expand (cdr prog))))))

(define (asm-labels xs org)
  (labels (((go p addr tbl)
      (cond ((null p) tbl)
            ((eq (caar p) 'label) (go (cdr p) addr (cons (list (cadr (car p)) addr) tbl)))
            (t (go (cdr p) (+ addr 1) tbl)))))
    (go xs org nil)))

(define (asm-resolve x tbl)
  (cond ((numberp x) x)
        ((and (consp x) (eq (car x) 'hi24))
         (mod (truncate (/ (asm-resolve (cadr x) tbl) 65536)) 256))
        ((and (consp x) (eq (car x) 'mid24))
         (mod (truncate (/ (asm-resolve (cadr x) tbl) 256)) 256))
        ((and (consp x) (eq (car x) 'lo24))
         (mod (asm-resolve (cadr x) tbl) 256))
        (t (let ((e (assoc x tbl)))
             (cond ((null e) (error (list 'asm-undefined-label x)))
                   (t (cadr e)))))))

(define (asm-encode stmt tbl)
  (cond ((eq (car stmt) 'word)
         (mod (asm-resolve (cadr stmt) tbl) WORD))
        (t (let ((op (cadr (assoc (car stmt) ISA2)))
                 (a (asm-resolve (nth-or (cdr stmt) 0) tbl))
                 (b (asm-resolve (nth-or (cdr stmt) 1) tbl)))
             (cond ((> a 255) (error (list 'asm-operand-too-big (car stmt) a)))
                   ((> b 255) (error (list 'asm-operand-too-big (car stmt) b)))
                   (t (+ (* op 65536) (* a 256) b)))))))

(define (assemble-at prog org)
  (let ((xs (asm-expand prog)))
    (let ((tbl (asm-labels xs org)))
      (map (lambda (s) (asm-encode s tbl))
           (filter (lambda (s) (not (eq (car s) 'label))) xs)))))
(define (assemble prog) (assemble-at prog 0))

(define (load-words m base words)
  (labels (((go w a) (cond ((null w) a)
                           (t (wr m a (car w)) (go (cdr w) (+ a 1))))))
    (go words base)))

(define (str-join2 parts)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (car parts) (str-join2 (cdr parts))))))

; ============================================================
; DEMOS + CHECKS
; ============================================================
(print "=== SISP MACHINE V2.1 ===")

; ---- 1. ALU --------------------------------------------------
(define c1 (mk-cpu))
(wr c1 0 32)
(load-words c1 32 (assemble '((loadi 0 7) (loadi 1 3) (add 0 1) (halt))))
(rw c1 FLG 2) (rw c1 PCI 32) (run1 c1 100 0)
(print (cat "ALU: 7+3=" (num-to-str (rr c1 0))))
(check "ALU: 7+3 = 10" (= (rr c1 0) 10))

; ---- 2. assembler: loop summing 1..5 with cmp/jne ------------
(define c2 (mk-cpu))
(load-words c2 0 (assemble
  '((loadi 0 1)              ; i = 1
    (loadi 1 0)              ; sum = 0
    (loadi 2 6)              ; limit+1
    (label lp)
    (add 1 0)
    (loadi 3 1) (add 0 3)
    (cmp 0 2)
    (jne lp)
    (loadi 4 200)            ; R4 = data address
    (store 4 1)              ; mem[200] = sum
    (halt))))
(rw c2 FLG 2) (rw c2 PCI 0) (run1 c2 500 0)
(print (cat "LOOP: sum 1..5 = " (num-to-str (rd c2 200))))
(check "assembler: sum 1..5 = 15 via cmp/jne loop" (= (rd c2 200) 15))

; ---- 3. condition codes: max of an array ----------------------
(define c3 (mk-cpu))
(load-words c3 0 (assemble
  '((loadi 0 100)            ; ptr
    (loadi 1 0)              ; best = 0
    (loadi 2 105)            ; end
    (loadi 4 1)              ; step
    (label lp)
    (load 3 0)               ; x = mem[ptr]
    (cmp 3 1)
    (jl skip)                ; x < best -> keep
    (mov 1 3)
    (label skip)
    (add 0 4)
    (cmp 0 2)
    (jne lp)
    (loadw 5 3841)           ; console print-number port
    (outb 5 1)
    (halt))))
(wr c3 100 37) (wr c3 101 12) (wr c3 102 88) (wr c3 103 5) (wr c3 104 60)
(rw c3 FLG 2) (rw c3 PCI 0) (run1 c3 500 0)
(check "cond codes: max(37,12,88,5,60) = 88" (= (rr c3 1) 88))

; ---- 4. shifts + logic: pack/unpack bytes ---------------------
(define c4 (mk-cpu))
(load-words c4 0 (assemble
  '((loadi 0 1)              ; byte a
    (loadi 1 2)              ; byte b
    (loadi 2 3)              ; byte c
    (loadi 3 8)
    (shl 1 3)                ; b << 8
    (shl 2 3) (shl 2 3)      ; c << 16
    (or 0 1) (or 0 2)        ; packed = a | b<<8 | c<<16
    (loadi 4 200)
    (store 4 0)
    ; unpack: mid = (packed >> 8) & 255
    (shr 0 3)                ; >> 8
    (loadi 5 255)
    (and 0 5)
    (loadi 4 201)
    (store 4 0)
    (loadi 6 85)             ; 0x55
    (loadi 1 170)            ; 0xAA
    (xor 6 1)                ; -> 255
    (not 6)                  ; -> 2^24-256
    (loadi 4 202)
    (store 4 6)
    (halt))))
(rw c4 FLG 2) (rw c4 PCI 0) (run1 c4 500 0)
(print (cat "PACK: word=" (num-to-str (rd c4 200))
            (cat " mid=" (num-to-str (rd c4 201)))))
(check "shifts/logic: pack 1|2<<8|3<<16 = 197121" (= (rd c4 200) 197121))
(check "shifts/logic: unpack mid byte = 2" (= (rd c4 201) 2))
(check "logic: not(xor(0x55,0xAA)) = 2^24-256" (= (rd c4 202) (- WORD 256)))

; ---- 5. console: character-true output ------------------------
(define c5 (mk-cpu))
(load-words c5 0 (assemble
  '((loadw 0 3840)           ; console TX port
    (loadi 1 72) (outb 0 1)  ; 'H'
    (loadi 1 69) (outb 0 1)  ; 'E'
    (loadi 1 76) (outb 0 1)  ; 'L'
    (loadi 1 76) (outb 0 1)  ; 'L'
    (loadi 1 79) (outb 0 1)  ; 'O'
    (loadi 1 10) (outb 0 1)  ; '\n' -> flushes the line
    (halt))))
(rw c5 FLG 2) (rw c5 PCI 0) (run1 c5 200 0)
(check "console: line buffer flushes on newline" (null con-line))

; ---- 6. MMU ---------------------------------------------------
(setq mmu-base 100) (setq mmu-limit 50)
(define t1 (mmu-trans 0)) (define t2 (mmu-trans 60))
(print (cat (cat (cat (cat "MMU: VA0->" (num-to-str (car t1)))
                 (cond ((cadr t1) " FAULT") (t ""))) " VA60->")
            (cond ((cadr t2) "FAULT") (t "OK"))))
(check "MMU: in-range relocated, out-of-range faults"
       (and (equal t1 (list 100 nil)) (cadr t2)))
(setq mmu-base 0) (setq mmu-limit 4096)

; ---- 7. disk: geometry + rewrite replaces ---------------------
(io-wr 3842 7) (setq dsk-buf (list 72 69 76)) (io-wr 3843 2)
(setq dsk-buf nil) (io-wr 3843 1)
(define got1 (list (io-rd 3844) (io-rd 3844) (io-rd 3844)))
(io-wr 3842 7) (setq dsk-buf (list 65 66)) (io-wr 3843 2)
(setq dsk-buf nil) (io-wr 3843 1)
(define got2 (list (io-rd 3844) (io-rd 3844)))
(print (cat "DSK: geometry " (num-to-str (io-rd 3845))
            (cat "x" (num-to-str (io-rd 3846)))))
(check "disk: wrote and read back bytes" (equal got1 (list 72 69 76)))
(check "disk: rewrite replaces the sector" (equal got2 (list 65 66)))
(check "disk: geometry ports report 64x256"
       (and (= (io-rd 3845) 64) (= (io-rd 3846) 256)))

; ---- 8. faults stop the machine --------------------------------
(define c8 (mk-cpu))
(rw c8 FLG 2) (rw c8 PCI 0)
(run1 c8 10 0)                       ; mem[0]=0 decodes as HALT: just halts
; An illegal opcode, by contrast, faults:
(define c8b (mk-cpu))
(wr c8b 0 (iw 99 0 0))
(rw c8b FLG 2) (rw c8b PCI 0)
(run1 c8b 10 0)
(print (cat "FLT: code=" (num-to-str (rr c8b FLT))))
(check "fault: illegal opcode sets FLT=1 and stops" (= (rr c8b FLT) 1))
(define c8c (mk-cpu))
(load-words c8c 0 (assemble '((loadi 0 7) (loadi 1 0) (div 0 1) (halt))))
(rw c8c FLG 2) (rw c8c PCI 0) (run1 c8c 10 0)
(check "fault: divide by zero sets FLT=2" (= (rr c8c FLT) 2))

; ---- 9. timer IRQ + IRET: main program survives interrupts -----
; Main sums 1..100 into R1 (5050).  Timer ISR bumps mem[250] and
; IRETs back.  If IRET did not restore PC+FLG, the sum would break.
; Main program lives at 32, past the vector table (mem[0..31]).
(define c9 (mk-cpu))
(load-words c9 32 (assemble-at
  '((loadi 0 1) (loadi 1 0) (loadi 2 101)
    (label lp)
    (add 1 0)
    (loadi 3 1) (add 0 3)
    (cmp 0 2)
    (jne lp)
    (loadi 4 201)
    (store 4 1)
    (halt)) 32))
(load-words c9 256 (assemble-at
  '((push 0) (push 1)
    (loadi 0 250)
    (load 1 0)
    (loadi 0 1)
    (add 1 0)
    (loadi 0 250)
    (store 0 1)
    (pop 1) (pop 0)
    (iret)) 256))
(wr c9 2 256)                          ; timer vector -> ISR at 256
(setq timr-intv 16)
(rw c9 FLG 6) (rw c9 IMSK 2) (rw c9 SPI 2048) (rw c9 PCI 32)
(run1 c9 20000 0)
(setq timr-intv 0)
(print (cat "IRQ: sum=" (num-to-str (rd c9 201))
            (cat " isr-ticks=" (num-to-str (rd c9 250)))))
(check "IRQ: main program result survives repeated interrupts"
       (= (rd c9 201) 5050))
(check "IRQ: timer ISR ran and returned via IRET repeatedly"
       (> (rd c9 250) 30))

; ---- 10. TAS ---------------------------------------------------
(define c10 (mk-cpu)) (wr c10 0 0)
(rw c10 0 0) (exec c10 (iw 20 0 0))    ; tas R0 (R0 = addr 0)
(print (cat (cat "TAS: old=" (num-to-str (rr c10 0)))
            (cat " new_val=" (num-to-str (rd c10 0)))))
(check "TAS: returns old value, sets cell to 1"
       (and (= (rr c10 0) 0) (= (rd c10 0) 1)))

; ---- 11. SYS hypercall: VM exit + resume ------------------------
(define c11 (mk-cpu))
(load-words c11 0 (assemble
  '((loadi 0 41)
    (sys 7)                  ; hypercall #7 -> FLT = 263
    (loadi 1 1)
    (add 0 1)
    (loadi 2 203)
    (store 2 0)
    (halt))))
(rw c11 FLG 2) (rw c11 PCI 0)
(run1 c11 100 0)
(check "SYS: hypercall stops the CPU with FLT=256+n" (= (rr c11 FLT) 263))
; host "kernel" services the call, then resumes the same CPU
(print (cat "KRN: servicing hypercall 7, R0=" (num-to-str (rr c11 0))))
(rw c11 FLT 0)
(run1 c11 100 0)
(check "SYS: after the kernel clears FLT the program resumes"
       (= (rd c11 203) 42))

; ---- 12. call/ret + push/pop ------------------------------------
(define c12 (mk-cpu))
(load-words c12 0 (assemble
  '((loadi 0 6)
    (call sq)                ; R1 = R0*R0
    (loadi 2 204)
    (store 2 1)
    (halt)
    (label sq)
    (push 0)
    (mov 1 0)
    (mul 1 0)
    (pop 0)
    (ret))))
(rw c12 FLG 2) (rw c12 SPI 2048) (rw c12 PCI 0) (run1 c12 200 0)
(check "call/ret: subroutine computes 6*6 = 36" (= (rd c12 204) 36))
(check "call/ret: stack balanced, R0 preserved" (= (rr c12 0) 6))

(print "=== V2.1 complete ===")
