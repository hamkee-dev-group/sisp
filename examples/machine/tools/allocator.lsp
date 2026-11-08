; ============================================================
; MEMORY ALLOCATOR FOR THE SISP MACHINE
;
; First-fit malloc/free for the CPU's 256-word RAM.  Free blocks
; store their size and next-pointer in the block itself (at the
; block's start address + 0 = size, + 1 = next-free address).
; Allocated blocks are returned as just the start address.
;
; The heap occupies RAM addresses HEAP_START..255.  Addresses
; 0..HEAP_START-1 are reserved for the kernel/program code.
;
; API:
;   (kalloc m size)   -> start address or nil (out of memory)
;   (kfree  m addr)   -> returns nil
;   (kheap-init m)     -> initialize the free list
; ============================================================

(define WORD  16777216)
(define MEM   256)
(define HEAP_START 128)
(define HEAP_END   255)

; ---- machine state (reuse from cpu.lsp) --------------------
(define (rd  m a) (truncate (mat-ref (car m) 0 a)))
(define (wr  m a v) (mat-set (car m) 0 a (mod v WORD)))
(define (rreg m r) (truncate (mat-ref (car (cdr m)) 0 r)))
(define (wreg m r v) (mat-set (car (cdr m)) 0 r (mod v WORD)))

; ---- free list stored in register R5 (allocator private) ----
(define FREE_HEAD 5)

; Get/set the size field of a free block at address addr.
; Block layout: [size, next_ptr, ... free space ...]
(define (block-size m addr) (rd m addr))
(define (block-next m addr) (rd m (+ addr 1)))
(define (block-set-size m addr sz) (wr m addr sz))
(define (block-set-next m addr nxt) (wr m (+ addr 1) nxt))

; Initialize the heap: one big free block from HEAP_START to HEAP_END.
; Each free block consumes 2 words of overhead (size, next).
(define (kheap-init m)
  (let ((total-size (- HEAP_END HEAP_START -1)))
    (block-set-size m HEAP_START (- total-size 2))
    (block-set-next m HEAP_START 0)      ; 0 = end of free list
    (wreg m FREE_HEAD HEAP_START)))

; Traverse the free list looking for a block >= needed size.
; Returns (prev-addr . block-addr) or nil.
(define (find-fit m prev curr needed)
  (cond ((= curr 0) nil)                           ; end of list
        ((>= (block-size m curr) needed)
         (cons prev curr))
        (t (find-fit m curr (block-next m curr) needed))))

; Allocate `words` words of RAM.  Returns start address or nil.
(define (kalloc m words)
  (let ((head (rreg m FREE_HEAD)))
    (let ((fit (find-fit m 0 head (+ words 2))))   ; +2 for header
      (cond ((eq fit nil) nil)
            (t (let ((prev (car fit)) (block (cdr fit)))
                 (let* ((total (block-size m block))
                        (needed (+ words 2))
                        (remainder (- total needed)))
                   (cond ((<= remainder 2)          ; too small to split
                          (cond ((= prev 0)
                                 (wreg m FREE_HEAD (block-next m block)))
                                (t (block-set-next m prev (block-next m block)))))
                         (t (let ((new-free (+ block needed)))
                              (block-set-size m new-free (- remainder 2))
                              (block-set-next m new-free (block-next m block))
                              (cond ((= prev 0)
                                     (wreg m FREE_HEAD new-free))
                                    (t (block-set-next m prev new-free))))))
                   (block-set-size m block needed)  ; mark as allocated
                   (+ block 2))))))))               ; skip header

; Free a previously allocated block.  Coalesces with adjacent free blocks.
(define (kfree m addr)
  (let ((block (- addr 2)))                         ; back to header
    (let* ((sz (block-size m block))
           (next-block (+ block sz))
           (prev-block (find-prev-free m block 0 (rreg m FREE_HEAD))))
      ; Check if next block is free and coalesce
      (let ((next-is-free (cond ((>= next-block HEAP_END) nil)
                                (t (is-free m next-block)))))
        (cond (next-is-free
               (block-set-size m block (+ sz (block-size m next-block)))
               (block-set-next m block (block-next m next-block)))
              (t (block-set-next m block
                   (cond ((= (rreg m FREE_HEAD) 0) 0)
                         (t (find-insert-after m block)))))))
      ; Check if previous block is free and coalesce
      (cond ((eq prev-block nil)                     ; insert at head
             (block-set-next m block (rreg m FREE_HEAD))
             (wreg m FREE_HEAD block))
            (t (let ((prev-end (+ prev-block (block-size m prev-block))))
                 (cond ((= prev-end block)           ; adjacent — coalesce
                        (block-set-size m prev-block
                          (+ (block-size m prev-block) sz))
                        (block-set-next m prev-block (block-next m block)))
                       (t (block-set-next m prev-block block)
                          (block-set-next m block (block-next m prev-block))))))))))

; Scan free list to find the block whose next pointer would contain `block`
(define (find-insert-after m block)
  (labels (((scan curr)
      (cond ((= curr 0) block)                      ; end, insert here
            ((< block curr) block)
            (t (scan (block-next m curr))))))
    (scan (rreg m FREE_HEAD))))

(define (find-prev-free m target prev curr)
  (cond ((= curr 0) nil)
        ((> curr target) (cond ((= prev 0) nil) (t prev)))
        (t (find-prev-free m target curr (block-next m curr)))))

(define (is-free m addr)
  (labels (((scan curr)
      (cond ((= curr 0) nil)
            ((= curr addr) t)
            (t (scan (block-next m curr))))))
    (scan (rreg m FREE_HEAD))))

; ---- demonstration ------------------------------------------
(define (make-test-cpu) (list (make-matrix 1 MEM) (make-matrix 1 10)))
(define dc (make-test-cpu))
(kheap-init dc)
; Allocate 10 words
(define a1 (kalloc dc 10))
; Allocate 5 words
(define a2 (kalloc dc 5))
; Free the first block
(kfree dc a1)
; Allocate 4 words (should fit in the freed 10-word block)
(define a3 (kalloc dc 4))
(list a1 a2 a3)   ; verify all allocations succeeded (non-nil)
