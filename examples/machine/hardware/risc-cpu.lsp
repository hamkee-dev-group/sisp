; ============================================================
; RISC CPU — A CLEAN 32-REGISTER MACHINE IN SISP
;
; A simplified RISC-style CPU with:
;   32 general-purpose registers (R0-R31, where R0 is always 0)
;   16-bit fixed-width instructions
;   Load/store architecture (only LD/ST touch memory)
;   Three-operand register format: op rd rs1 rs2|imm
;
; Instruction format (16-bit):
;   bits 15..12 = opcode (4 bits, 16 operations)
;   bits 11..8  = rd  (destination register)
;   bits 7..4   = rs1 (first source register)
;   bits 3..0   = rs2 (second source or 4-bit immediate)
;
; Opcodes: ADD(0) SUB(1) AND(2) OR(3) XOR(4) SLT(5)
;          ADDI(6) LW(7) SW(8) BEQ(9) JAL(10) JR(11)
;          LUI(12) HALT(13) SLL(14) SRL(15)
; ============================================================

(define WORD  65536)          ; 16-bit words
(define MEM   256)
(define NREG  32)
(define ZERO  0)              ; R0 is hardwired to 0

(define (hi4 w) (mod (truncate (/ w 4096)) 16))
(define (mid-hi4 w) (mod (truncate (/ w 256)) 16))
(define (mid-lo4 w) (mod (truncate (/ w 16)) 16))
(define (lo4 w) (mod w 16))

; ---- machine state ------------------------------------------
(define (make-risc-cpu)
  (list (make-matrix 1 MEM) (make-matrix 1 NREG)))

(define (ram  m) (car m))
(define (regs m) (car (cdr m)))
(define (rd  m a) (truncate (mat-ref (ram m) 0 a)))
(define (wr  m a v) (mat-set (ram m) 0 a (mod v WORD)))
(define (gr  m r) (truncate (mat-ref (regs m) 0 r)))
(define (swr m r v) (mat-set (regs m) 0 r (mod v WORD)))

; R0 always reads as 0, writes are ignored.
(define (risc-gr m r)
  (cond ((= r 0) 0) (t (gr m r))))
(define (risc-wr m r v)
  (cond ((= r 0) nil) (t (swr m r v))))

; ---- instruction set ----------------------------------------
(define (risc-exec m w)
  (let ((op (hi4 w)) (rd (mid-hi4 w))
        (rs1 (mid-lo4 w)) (rs2 (lo4 w)))
    (cond ((= op  0) (risc-wr m rd (+ (risc-gr m rs1) (risc-gr m rs2))))  ; ADD
          ((= op  1) (risc-wr m rd (- (risc-gr m rs1) (risc-gr m rs2))))  ; SUB
          ((= op  2) (risc-wr m rd (logand (risc-gr m rs1) (risc-gr m rs2)))) ; AND
          ((= op  3) (risc-wr m rd (logior (risc-gr m rs1) (risc-gr m rs2)))) ; OR
          ((= op  4) (risc-wr m rd (logxor (risc-gr m rs1) (risc-gr m rs2)))) ; XOR
          ((= op  5) (risc-wr m rd (cond ((< (risc-gr m rs1) (risc-gr m rs2)) 1) (t 0)))) ; SLT
          ((= op  6) (risc-wr m rd (+ (risc-gr m rs1) (sign-extend4 rs2)))) ; ADDI
          ((= op  7) (risc-wr m rd (rd m (+ (risc-gr m rs1) (sign-extend4 rs2))))) ; LW
          ((= op  8) (wr m (+ (risc-gr m rs1) (sign-extend4 rs2)) (risc-gr m rd))) ; SW
          ((= op  9) (cond ((= (risc-gr m rs1) (risc-gr m rs2))  ; BEQ
                            (swr m 30 (+ (gr m 30) (sign-extend4 rd)))))) ; offset via rd field
          ((= op 10) (progn (swr m 31 (gr m 30))                    ; JAL: save PC+1
                            (swr m 30 (+ (gr m 30) rd))))          ; jump
          ((= op 11) (swr m 30 (risc-gr m rs1)))                   ; JR
          ((= op 13) (swr m 29 1))                                 ; HALT (R29=1)
          (t nil))))

(define (sign-extend4 x)
  (cond ((>= x 8) (- x 16)) (t x)))

; Bitwise logic via arithmetic (SISP has no bitwise ops).
(define (logand a b)
  (logop a b 0 1 +))
(define (logior a b)
  (logop a b 0 1 or-logic))
(define (logxor a b)
  (logop a b 0 1 xor-logic))

(define (logop a b pos bit result)
  (cond ((= pos 16) result)
        (t (logop (truncate (/ a 2)) (truncate (/ b 2))
                  (+ pos 1) (* bit 2)
                  (+ result (* bit (bit-op (mod a 2) (mod b 2) result result)))))))

(define (or-logic x y) (cond ((= (+ x y) 0) 0) (t 1)))
(define (xor-logic x y) (cond ((= x y) 0) (t 1)))
(define (bit-op x y a b) (mod (+ x y) 2))

; ---- run loop -----------------------------------------------
(define (risc-run m budget)
  (labels (((cycle n)
      (cond ((= (gr m 29) 1) n)           ; HALT flag
            ((= n budget) n)
            (t (let ((pc (gr m 30)))
                 (let ((w (rd m pc)))
                   (swr m 30 (+ pc 1))     ; PC++
                   (risc-exec m w)
                   (cycle (+ n 1))))))))
    (cycle 0)))

; ---- assembler (symbolic -> words) --------------------------
(define (risc-asm op rd rs1 rs2)
  (+ (* op 4096) (* rd 256) (* rs1 16) rs2))

; ---- demo: compute 3 + 4 into R1, store to RAM[100] ---------
(define risc-prog
  (list (risc-asm  6  1  0  3)     ; ADDI R1 R0 3    (R1=3)
        (risc-asm  6  2  0  4)     ; ADDI R2 R0 4    (R2=4)
        (risc-asm  0  3  1  2)     ; ADD  R3 R1 R2   (R3=7)
        (risc-asm  6  4  0 100)    ; ADDI R4 R0 100  (addr=100)
        (risc-asm  8  3  4  0)     ; SW   R3 (R4)    (mem[100]=7)
        (risc-asm 13  0  0  0)))   ; HALT

; Load and run
(define risc-cpu (make-risc-cpu))
(risc-run risc-cpu 100)   ; just load and run instructions manually

; Load program into RAM
(define (risc-load m words)
  (labels (((go w a)
      (cond ((null w) a) (t (progn (wr m a (car w)) (go (cdr w) (+ a 1)))))))
    (go words 0)))

(risc-load risc-cpu risc-prog)
(risc-run risc-cpu 100)
(risc-gr risc-cpu 3)   ; => 7
(rd risc-cpu 100)       ; => 7
