; ============================================================
; PROGRAM LOADER — EXECUTABLE FORMAT FOR THE SISP OS
;
; Defines an executable format (simple header + code segment),
; loads programs from the filesystem into RAM, and prepares
; them for execution.  Integrates with the file system and the
; memory allocator.
;
; Executable format (stored as a list):
;   (name entry-addr (word word ...) (data-item ...))
; where data items are (addr . value) pairs written to RAM
; before execution.
; ============================================================

; ---- machine primitives ------------------------------------
(define WORD 16777216) (define MEM 256) (define PCI 8)

(define (rd m a) (truncate (mat-ref (car m) 0 a)))
(define (wr m a v) (mat-set (car m) 0 a (mod v WORD)))
(define (rreg m r) (truncate (mat-ref (car (cdr m)) 0 r)))
(define (wreg m r v) (mat-set (car (cdr m)) 0 r (mod v WORD)))

; ---- executable format -------------------------------------
(define (exe-name   exe) (car exe))
(define (exe-entry  exe) (cadr exe))
(define (exe-code   exe) (caddr exe))
(define (exe-data   exe) (cadddr exe))

; Build an executable.
(define (make-exe name entry code data-seg)
  (list name entry code data-seg))

; ---- loader ------------------------------------------------
; Load an executable into RAM at the given base address.
; Returns the CPU with the program loaded and PC set to entry.
(define (load-exe cpu exe base)
  (let ((code (exe-code exe))
        (data (exe-data exe))
        (entry (exe-entry exe)))
    ; Write code segment
    (load-code cpu code base)
    ; Write data segment
    (load-data cpu data)
    ; Set PC to entry (relative to base)
    (wreg cpu PCI (+ base entry))
    cpu))

(define (load-code cpu code base)
  (labels (((go w addr)
      (cond ((null w) nil)
            (t (progn (wr cpu (+ base addr) (car w))
                      (go (cdr w) (+ addr 1)))))))
    (go code 0)))

(define (load-data cpu data)
  (cond ((null data) nil)
        (t (progn (wr cpu (caar data) (cdar data))
                  (load-data cpu (cdr data))))))

; ---- linker: resolve a list of object files into one exe ----
; Each object file is (words . syms) from the linker format.
; Simple concatenation with base-address computation.

(define (link-objects objs)
  (link-objs objs 0 nil nil nil))

(define (link-objs objs base all-code all-data all-syms)
  (cond ((null objs)
         (make-exe "linked"
                   (cdr (assoc 'main all-syms))
                   (rev-append all-code nil)
                   all-data))
        (t (let* ((obj   (car objs))
                  (words (car obj))
                  (syms  (cdr obj))
                  (next  (+ base (length words))))
             (link-objs (cdr objs) next
               (append all-code words)
               all-data
               (append (relocate-syms syms base) all-syms))))))

(define (relocate-syms syms base)
  (map (lambda (e) (cons (car e) (+ base (cdr e)))) syms))

(define (rev-append lst acc)
  (cond ((null lst) acc)
        (t (rev-append (cdr lst) (cons (car lst) acc)))))

; ---- demo: a simple program ---------------------------------
(define hello-exe
  (make-exe "hello" 0
    (list (+ (* 1 65536) (* 0 256) 72)       ; loadi r0 72 ('H')
          (+ (* 11 65536) (* 0 256) 0)       ; out r0
          0)                                  ; halt
    nil))                                     ; no data

(define test-cpu (list (make-matrix 1 MEM) (make-matrix 1 10)))
(load-exe test-cpu hello-exe 0)
(rreg test-cpu 0)   ; => 72
