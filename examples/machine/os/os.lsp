; ============================================================
; A MULTITASKING OS BUILT IN SISP
;
; A preemptive round-robin kernel running several programs on the
; SISP machine (see cpu.lsp).  Each process owns its own RAM and
; register file (two matrices), so a context switch is just a
; matter of which process's matrices the CPU steps next -- no
; save/restore code at all.  The kernel time-slices with a fixed
; quantum, so unrelated processes interleave; a SYS instruction
; traps into the kernel for exit / yield / disk-write.
;
; Showcases: matrices as per-process address spaces, tail-recursive
; scheduler and cycle loop, closures/assoc for the process table
; and disk, exact integer arithmetic for instruction decode,
; strings for the scheduler trace.
; ============================================================

; ---- word geometry + machine state (as in cpu.lsp) ---------
(define WORD 16777216)
(define MEM  256)
(define NREG 11)                 ; R0..R7, PC(8), HALT(9), TRAP(10)
(define PCI 8) (define HLI 9) (define TRI 10)

(define (byte-hi w) (mod (truncate (/ w 65536)) 256))
(define (byte-mid w) (mod (truncate (/ w 256)) 256))
(define (byte-lo w) (mod w 256))

(define (make-cpu) (list (make-matrix 1 MEM) (make-matrix 1 NREG)))
(define (ram m) (car m))
(define (regs m) (car (cdr m)))
(define (rd-mem m a) (truncate (mat-ref (ram m) 0 a)))
(define (wr-mem m a v) (mat-set (ram m) 0 a (mod v WORD)))
(define (get-reg m r) (truncate (mat-ref (regs m) 0 r)))
(define (set-reg m r v) (mat-set (regs m) 0 r (mod v WORD)))
(define (get-pc m) (truncate (mat-ref (regs m) 0 PCI)))
(define (set-pc m v) (mat-set (regs m) 0 PCI v))
(define (haltedp m) (= (truncate (mat-ref (regs m) 0 HLI)) 1))
(define (do-halt m) (mat-set (regs m) 0 HLI 1))
(define (get-trap m) (truncate (mat-ref (regs m) 0 TRI)))
(define (set-trap m c) (mat-set (regs m) 0 TRI (+ c 1)))   ; store code+1
(define (clear-trap m) (mat-set (regs m) 0 TRI 0))

; ---- instruction set (adds SYS) ----------------------------
(define ISA
  (list (list 'halt 0 0) (list 'loadi 1 2) (list 'add 2 2) (list 'sub 3 2)
        (list 'mul 4 2) (list 'mov 5 2) (list 'load 6 2) (list 'store 7 2)
        (list 'jmp 8 1) (list 'jz 9 2) (list 'jnz 10 2) (list 'out 11 1)
        (list 'inc 12 1) (list 'dec 13 1) (list 'sys 14 1)))
(define (isa-op name) (car (cdr (assoc name ISA))))

(define (rev lst)
  (labels (((go l acc) (cond ((null l) acc) (t (go (cdr l) (cons (car l) acc)))))) (go lst nil)))
(define (nth-or ops i)
  (cond ((null ops) 0) ((= i 0) (car ops)) (t (nth-or (cdr ops) (- i 1)))))
(define (str-join parts)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (car parts) (str-join (cdr parts))))))

; ---- assembler ---------------------------------------------
(define (asm-labels prog)
  (labels (((go p addr tbl)
      (cond ((null p) tbl)
            ((eq (car (car p)) 'label)
               (go (cdr p) addr (cons (list (car (cdr (car p))) addr) tbl)))
            (t (go (cdr p) (+ addr 1) tbl)))))
    (go prog 0 nil)))
(define (resolve x tbl)
  (cond ((numberp x) x) (t (car (cdr (assoc x tbl))))))
(define (encode stmt tbl)
  (let ((op (isa-op (car stmt)))
        (a (resolve (nth-or (cdr stmt) 0) tbl))
        (b (resolve (nth-or (cdr stmt) 1) tbl)))
    (+ (* op 65536) (+ (* a 256) b))))
(define (assemble prog)
  (let ((tbl (asm-labels prog)))
    (labels (((go p acc)
        (cond ((null p) (rev acc))
              ((eq (car (car p)) 'label) (go (cdr p) acc))
              (t (go (cdr p) (cons (encode (car p) tbl) acc))))))
      (go prog nil))))
(define (load-image m words)
  (labels (((go w a) (cond ((null w) a) (t (progn (wr-mem m a (car w)) (go (cdr w) (+ a 1)))))))
    (go words 0)))

; ---- kernel globals ----------------------------------------
(define CURPID 0)                ; pid whose slice is running (for OUT tag)
(define DISK nil)                ; assoc "filesystem": (name value) pairs

; ---- execute one instruction -------------------------------
; OUT prints inline (tagged with the running pid); SYS traps.
(define (exec m w)
  (let ((op (byte-hi w)) (a (byte-mid w)) (b (byte-lo w)))
    (cond ((= op 0)  (do-halt m))
          ((= op 1)  (set-reg m a b))
          ((= op 2)  (set-reg m a (+ (get-reg m a) (get-reg m b))))
          ((= op 3)  (set-reg m a (- (get-reg m a) (get-reg m b))))
          ((= op 4)  (set-reg m a (* (get-reg m a) (get-reg m b))))
          ((= op 5)  (set-reg m a (get-reg m b)))
          ((= op 6)  (set-reg m a (rd-mem m (get-reg m b))))
          ((= op 7)  (wr-mem m (get-reg m a) (get-reg m b)))
          ((= op 8)  (set-pc m a))
          ((= op 9)  (cond ((= (get-reg m a) 0) (set-pc m b)) (t nil)))
          ((= op 10) (cond ((= (get-reg m a) 0) nil) (t (set-pc m b))))
          ((= op 11) (print (str-join (list "      P" (num-to-str CURPID)
                                            " out> " (num-to-str (get-reg m a))))))
          ((= op 12) (set-reg m a (+ (get-reg m a) 1)))
          ((= op 13) (set-reg m a (- (get-reg m a) 1)))
          ((= op 14) (set-trap m a))          ; SYS a  -> trap to kernel
          (t nil))))

; ---- run one time-slice: up to `q` cycles ------------------
; Stops early on HALT or a SYS trap; returns (reason . code/cycles).
(define (run-quantum m q)
  (labels (((cyc n)
      (cond ((haltedp m) (list 'halt n))
            ((> (get-trap m) 0) (list 'trap (- (get-trap m) 1)))
            ((= n q) (list 'quantum n))
            (t (let ((pc (get-pc m)))
                 (let ((w (rd-mem m pc)))
                   (set-pc m (+ pc 1))
                   (exec m w)
                   (cyc (+ n 1))))))))
    (cyc 0)))

; ---- process table + scheduler -----------------------------
(define (make-proc pid cpu) (list pid cpu))
(define (proc-pid p) (car p))
(define (proc-cpu p) (car (cdr p)))
(define (banner p msg) (print (str-join (list "  [kernel] P" (num-to-str (proc-pid p)) " " msg))))

(define (do-put p)               ; SYS 2 : DISK[R1] <- R0
  (let ((cpu (proc-cpu p)))
    (let ((name (get-reg cpu 1)) (val (get-reg cpu 0)))
      (setq DISK (cons (list name val) DISK))
      (banner p (str-join (list "wrote disk[" (num-to-str name) "] = " (num-to-str val)))))))

; A slice finished: decide the queue for the next round.
(define (dispatch-trap p rest code)
  (cond ((= code 0) (progn (banner p "exit") rest))
        ((= code 1) (progn (clear-trap (proc-cpu p)) (banner p "yield")
                           (append rest (list p))))
        ((= code 2) (progn (do-put p) (clear-trap (proc-cpu p))
                           (append rest (list p))))
        (t rest)))

(define (dispatch p rest r)
  (let ((why (car r)))
    (cond ((eq why 'halt) (progn (banner p "halted") rest))
          ((eq why 'quantum) (progn (banner p "preempted (quantum expired)")
                                    (append rest (list p))))
          ((eq why 'trap) (dispatch-trap p rest (car (cdr r))))
          (t rest))))

(define (schedule queue quantum)
  (labels (((loop q)
      (cond ((null q) (print "  [kernel] all processes finished"))
            (t (progn
                 (setq CURPID (proc-pid (car q)))
                 (loop (dispatch (car q) (cdr q)
                                 (run-quantum (proc-cpu (car q)) quantum))))))))
    (loop queue)))

; ---- boot two programs -------------------------------------
; P1 counts 1,2,3 then stores 42 to disk file 1, then exits.
(define prog-count
  (list '(loadi 0 0) '(loadi 1 1) '(loadi 2 3)   ; i=0 one=1 limit=3
        '(label lp)
        '(add 0 1) '(out 0)                        ; i++ ; print i
        '(mov 3 0) '(sub 3 2) '(jnz 3 lp)          ; loop while i!=limit
        '(loadi 0 42) '(loadi 1 1) '(sys 2)        ; disk[1] = 42
        '(sys 0) '(halt)))

; P2 prints the squares 1,4,9 then stores 99 to disk file 2, exits.
(define prog-squares
  (list '(loadi 0 0) '(loadi 1 1) '(loadi 2 3)
        '(label lp)
        '(add 0 1) '(mov 4 0) '(mul 4 0) '(out 4)  ; i++ ; sq=i*i ; print sq
        '(mov 3 0) '(sub 3 2) '(jnz 3 lp)
        '(loadi 0 99) '(loadi 1 2) '(sys 2)        ; disk[2] = 99
        '(sys 0) '(halt)))

(define (boot pid src)
  (let ((cpu (make-cpu))) (load-image cpu (assemble src)) (make-proc pid cpu)))

(print "=== A multitasking OS built in SISP ===")
(print "Two processes, round-robin, quantum = 4 cycles.")
(print "Watch P1 (counter) and P2 (squares) interleave:")
(print "")
(schedule (list (boot 1 prog-count) (boot 2 prog-squares)) 4)
(print "")
(print (str-join (list "Kernel disk (filesystem) after run: ")))
(print DISK)
(print "=== done ===")
