; ============================================================
; FORTH-LIKE STACK LANGUAGE — COMPILED TO THE SISP MACHINE
;
; A tiny concatenative language that compiles directly to the
; SISP CPU's instruction set.  Programs are lists of words,
; each word is either a primitive or a user-defined subroutine.
;
; Supported primitives:
;   literals   push the number
;   dup drop   stack shuffling
;   + - *      arithmetic
;   dot        print top of stack.  Forth spells this ".", but "."
;              is not a legal SISP symbol, so it is spelled "dot".
;
; Control flow (if..then, begin..while..repeat) and user-defined
; words are NOT implemented yet; compile-token leaves anything it
; does not recognise as a no-op.
;
; The data stack lives in RAM at addresses 240-255 (grows down).
; The return stack is at addresses 224-239.
; Stack pointer is R6, return-stack pointer is R7.
; ============================================================

(define WORD  16777216)
(define MEM   256)
(define NREG  10)
(define PCI   8)
(define HLI   9)
(define DSP   6)        ; data stack pointer register
(define RSP   7)        ; return stack pointer register
(define DS_BASE 255)
(define RS_BASE 239)

; ---- machine primitives (subset) ---------------------------
(define (rd  m a) (truncate (mat-ref (car m) 0 a)))
(define (wr  m a v) (mat-set (car m) 0 a (mod v WORD)))
(define (rreg m r) (truncate (mat-ref (car (cdr m)) 0 r)))
(define (wreg m r v) (mat-set (car (cdr m)) 0 r (mod v WORD)))

; ---- ISA helpers -------------------------------------------
(define (isa-op name)
  (car (cdr (assoc name
    '((halt 0 0) (loadi 1 2) (add 2 2) (sub 3 2) (mul 4 2)
      (mov 5 2) (load 6 2) (store 7 2) (jmp 8 1) (jz 9 2)
      (jnz 10 2) (out 11 1) (inc 12 1) (dec 13 1))))))

; Encode an instruction word: op*65536 + a*256 + b
(define (instr op a b)
  (+ (* op 65536) (* a 256) b))

; ---- forth compiler state ----------------------------------
; The compiler tracks:
;   output  — list of emitted machine words (in reverse)
;   words   — dictionary: alist of (name . address)
;   next-addr — next free address in the code segment
;   labels  — list of pending forward references (name . patch-addr)

(define (emit state word)
  (let ((out (car state))
        (wa  (car (cdr state)))
        (na  (car (cdr (cdr state))))
        (lb  (car (cdr (cdr (cdr state))))))
    (list (cons word out) wa (+ na 1) lb)))

(define (define-word state name addr)
  (let ((out (car state))
        (wa  (car (cdr state)))
        (na  (car (cdr (cdr state))))
        (lb  (car (cdr (cdr (cdr state))))))
    (list out (cons (list name addr) wa) na lb)))

(define (add-label state name)
  (let ((out (car state))
        (wa  (car (cdr state)))
        (na  (car (cdr (cdr state))))
        (lb  (car (cdr (cdr (cdr state))))))
    (list out wa na (cons (list name na) lb))))

; Look up a word: first in the dictionary, then in pending labels.
(define (lookup-wa state name)
  (let ((wa (car (cdr state)))
        (lb (car (cdr (cdr (cdr state))))))
    (let ((e (assoc name wa)))
      (cond ((not (null e)) (car (cdr e)))
            (t (let ((l (assoc name lb)))
                 (cond ((not (null l)) (car (cdr l)))
                       (t nil))))))))

(define (rev lst)
  (labels (((go l a) (cond ((null l) a) (t (go (cdr l) (cons (car l) a)))))) (go lst nil)))

; ---- forth compiler proper ---------------------------------
; Compile a forth program (list of tokens) into machine words.
; Returns a list of assembled words.

(define (forth-pass tokens state)
  (cond ((null tokens) state)
        (t (forth-pass (cdr tokens)
             (compile-token (car tokens) state)))))

; Emit a whole list of words in order.
(define (emit-all state words)
  (foldl emit state words))

; Every token leaves the stack in the same shape it found it, minus
; whatever it consumed: values live on the data stack, r0/r1 are only
; ever scratch between a pop and the matching push.
(define (compile-token token state)
  (cond ((numberp token)
         (emit-all state (list (instr 1 0 token)     ; loadi r0 token
                               (instr 14 0 0))))     ; push  r0
        ((eq token 'dup)
         (emit-all state (list (instr 15 0 0)        ; pop   r0
                               (instr 14 0 0)        ; push  r0
                               (instr 14 0 0))))     ; push  r0 again
        ((eq token 'drop)
         (emit state (instr 15 0 0)))                ; pop -> discard
        ((eq token '+)  (emit-binop state 2))        ; add
        ((eq token '-)  (emit-binop state 3))        ; sub
        ((eq token '*)  (emit-binop state 4))        ; mul
        ((eq token 'dot)
         (emit-all state (list (instr 15 0 0)        ; pop r0
                               (instr 11 0 0))))     ; out r0
        (t state)))

; pop the two operands (top of stack is the right-hand side), combine
; into r0, push the result back
(define (emit-binop state op)
  (emit-all state (list (instr 15 1 0)               ; pop  r1  (rhs)
                        (instr 15 0 0)               ; pop  r0  (lhs)
                        (instr op 0 1)               ; op   r0, r1
                        (instr 14 0 0))))            ; push r0

(define (forth-compile tokens)
  (let ((state (list nil nil 0 nil)))
    (let ((final (forth-pass tokens state)))
      (rev (car final)))))

; ---- demo: forth program "3 4 + ." (prints 7) -------------
(define prog-words (forth-compile '(3 4 + dot)))
(print "Forth compiled to:")
(print prog-words)

(print "--- Checks ---")

(check "instruction words pack op/a/b into 24 bits"
       (and (= (instr 2 0 1) 131073)
            (and (= (instr 1 0 3) 65539) (= (instr 14 0 0) 917504))))
(check "the ISA table gives each word its opcode"
       (and (= (isa-op 'add) 2) (and (= (isa-op 'halt) 0) (= (isa-op 'out) 11))))

(check "a literal loads then pushes"
       (equal (forth-compile '(7))
              (list (instr 1 0 7) (instr 14 0 0))))
(check "dup replaces one stack cell with two"
       (equal (forth-compile '(dup))
              (list (instr 15 0 0) (instr 14 0 0) (instr 14 0 0))))
(check "drop just pops" (equal (forth-compile '(drop)) (list (instr 15 0 0))))
(check "a binary operator pops both operands and pushes the result"
       (equal (forth-compile '(+))
              (list (instr 15 1 0) (instr 15 0 0) (instr 2 0 1) (instr 14 0 0))))
(check "- and * differ only in the opcode"
       (and (equal (forth-compile '(-))
                   (list (instr 15 1 0) (instr 15 0 0) (instr 3 0 1) (instr 14 0 0)))
            (equal (forth-compile '(*))
                   (list (instr 15 1 0) (instr 15 0 0) (instr 4 0 1) (instr 14 0 0)))))
(check "dot pops the value it prints"
       (equal (forth-compile '(dot)) (list (instr 15 0 0) (instr 11 0 0))))
(check "an empty program compiles to nothing" (null (forth-compile nil)))
(check "an unrecognised word emits no code" (null (forth-compile '(nosuchword))))

(check "3 4 + dot compiles to the expected image"
       (equal prog-words
              (list (instr 1 0 3) (instr 14 0 0)      ; 3
                    (instr 1 0 4) (instr 14 0 0)      ; 4
                    (instr 15 1 0) (instr 15 0 0)     ; +
                    (instr 2 0 1) (instr 14 0 0)
                    (instr 15 0 0) (instr 11 0 0))))  ; dot
(check "the program is stack-balanced: 5 pushes, 5 pops"
       (let ((ops (map (lambda (w) (mod (truncate (/ w 65536)) 256)) prog-words)))
         (= (count (lambda (o) (= o 14)) ops)
            (count (lambda (o) (= o 15)) ops))))

; Compiler state: (output dictionary next-address labels)
(check "emit advances the next free address"
       (let ((st (emit (emit (list nil nil 0 nil) 1) 2)))
         (and (= (car (cdr (cdr st))) 2) (equal (car st) (list 2 1)))))
(check "a defined word can be looked up by name"
       (= (lookup-wa (define-word (list nil nil 0 nil) 'square 40) 'square) 40))
(check "a pending label can be looked up by name"
       (= (lookup-wa (add-label (list nil nil 12 nil) 'loop) 'loop) 12))
(check "an unknown word is not found"
       (null (lookup-wa (list nil nil 0 nil) 'nowhere)))
(print "")
(print "=== Forth compiler checks complete ===")
