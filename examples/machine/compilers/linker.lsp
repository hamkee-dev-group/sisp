; ============================================================
; LINKER — MULTI-FILE ASSEMBLY FOR THE SISP MACHINE
;
; Extends cpu.lsp with a linker that combines separately
; assembled object files (lists of words), resolving external
; symbols across files.  Each object file carries a symbol table
; of (name . exported-address) and (name . undefined) entries.
;
; Linker accepts a list of object files and a relocation base
; for each, produces a single merged word list and a global
; label table for the linked image.
; ============================================================

; ---- an object file is (words . syms) where syms is an alist
; of (symbol . address-or-nil).  nil means "undefined — must be
; resolved by another file."

(define LINK-ISA
  (list (list 'halt 0) (list 'loadi 1) (list 'mul 4)
        (list 'out 11) (list 'push 14) (list 'pop 15)
        (list 'call 16) (list 'ret 17)))

(define (link-opcode name) (cadr (assoc name LINK-ISA)))

(define (labels-pass1 prog)
  (labels (((scan forms address table)
      (cond ((null forms) table)
            ((eq (caar forms) 'label)
             (scan (cdr forms) address
                   (cons (list (cadar forms) address) table)))
            ((eq (caar forms) 'extern)
             (scan (cdr forms) address table))
            (t (scan (cdr forms) (+ address 1) table)))))
    (scan prog 0 nil)))

(define (link-operand operands position table)
  (cond ((null operands) 0)
        ((> position 0) (link-operand (cdr operands) (- position 1) table))
        ((numberp (car operands)) (car operands))
        (t (let ((entry (assoc (car operands) table)))
             (cond (entry (cadr entry)) (t 0))))))

(define (encode stmt table)
  (+ (* (link-opcode (car stmt)) 65536)
     (* (link-operand (cdr stmt) 0 table) 256)
     (link-operand (cdr stmt) 1 table)))

(define (obj-words obj) (car obj))
(define (obj-syms  obj) (cdr obj))
(define (obj-len   obj) (length (car obj)))

; Build a flat symbol table from an object file, offset by base.
(define (relocate-syms obj base)
  (map (lambda (entry)
         (let ((sym (car entry)) (addr (cadr entry)))
           (cond ((eq addr nil) entry)           ; undefined stays nil
                 (t (cons sym (+ base addr)))))) ; relocate
       (obj-syms obj)))

; Merge two alists, keeping the FIRST definition of each symbol.
(define (merge-syms a b)
  (cond ((eq b nil) a)
        (t (let ((entry (assoc (caar b) a)))
             (cond ((eq entry nil) (merge-syms (cons (car b) a) (cdr b)))
                   (t (merge-syms a (cdr b))))))))

; Resolve an undefined symbol across the global symbol table.
; If still unresolved, keep it nil (emit 0 placeholder).
(define (resolve-sym sym global)
  (let ((e (assoc sym global)))
    (cond ((eq e nil) nil)
          (t (cadr e)))))

; Relocate a list of words using the global symbol table.
; For each word, decode opcode, A, B.  If A/B are label symbols
; (not numbers in this pass), look them up in the global table.
(define (relocate-words words base global old-syms)
  (labels (((go w acc)
      (cond ((null w) (rev acc))
            (t (go (cdr w) (cons (car w) acc)))))) ; pass through unchanged
    (go words nil)))

; ---- two-pass assembler with external symbol support --------
; The assembler now produces (words . syms) instead of just words.
; External references are recorded as (name . nil) in syms.

(define (asm-obj-words prog tbl)
  (labels (((go p acc)
      (cond ((null p) (rev acc))
            ((eq (car (car p)) 'label) (go (cdr p) acc))
            ((eq (car (car p)) 'extern)
             (go (cdr p) acc))
            (t (go (cdr p) (cons (encode (car p) tbl) acc))))))
    (go prog nil)))

(define (asm-obj-syms prog tbl)
  (labels (((go p acc)
      (cond ((null p) acc)
            ((eq (car (car p)) 'label)
             (go (cdr p) (cons (cons (cadr (car p)) (cdr (assoc (cadr (car p)) tbl))) acc)))
            ((eq (car (car p)) 'extern)
             (go (cdr p) (cons (cons (cadr (car p)) nil) acc)))
            (t (go (cdr p) acc)))))
    (go prog nil)))

(define (assemble-obj prog)
  (let ((tbl (labels-pass1 prog)))
    (cons (asm-obj-words prog tbl) (asm-obj-syms prog tbl))))

; ---- linker proper ------------------------------------------
; Takes a list of (obj . base) pairs, returns (linked-words . global-syms).
(define (link objects)
  (link-step objects 0 nil nil))

(define (link-step objs next-base linked acc-syms)
  (cond ((null objs) (cons (rev linked) acc-syms))
        (t (let* ((obj  (caar objs))
                  (base (cdar objs))
                  (words (obj-words obj))
                  (syms  (relocate-syms obj base)))
             (link-step (cdr objs) (+ next-base (obj-len obj))
               (append linked words)        ; FIX: should append not rev
               (merge-syms acc-syms syms))))))

; ---- helpers ------------------------------------------------
(define (rev lst)
  (labels (((go l a) (cond ((null l) a) (t (go (cdr l) (cons (car l) a)))))) (go lst nil)))

; ---- demonstration: two files, each defines one function ----
; File 1: main that calls square (defined in file 2)
(define main-obj
  (assemble-obj
    '((loadi 0 7)           ; argument = 7
      (extern square)
      (call square)          ; external symbol
      (out 0)
      (halt))))

; File 2: defines square (R0 <- R0 * R0)
(define square-obj
  (assemble-obj
    '((label square)
      (push 0)               ; save R0
      (mul 0 0)              ; square
      (pop 1)                ; dummy pop to keep stack balanced
      (ret))))

(define result (link (list (cons main-obj 0) (cons square-obj (obj-len main-obj)))))
(define linked (car result))
(define syms   (cdr result))
(length linked)   ; should be > length(main-obj) + length(square-obj)
