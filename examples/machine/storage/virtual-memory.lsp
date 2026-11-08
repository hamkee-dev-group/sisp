; ============================================================
; VIRTUAL MEMORY — page tables, TLB, swap, COW fork
;
; Each process has a page table mapping virtual pages to
; physical frames.  The MMU translates on every load/store.
; Page faults trigger the kernel to swap in from disk.
; Copy-on-write fork shares pages until a write occurs.
; ============================================================

(define (str-join parts sep)
  (cond ((null parts) "") ((null (cdr parts)) (car parts))
        (t (cat (cat (car parts) sep) (str-join (cdr parts) sep)))))

; ---- Page/frame constants -----------------------------------
(define PAGE_SIZE 16)        ; words per page
(define NUM_FRAMES 16)       ; physical frames in RAM (16 * 16 = 256 words)
(define SWAP_DISK nil)       ; (page-id . data) alist on block disk

; ---- Frame allocator ----------------------------------------
(define (make-list n v) (cond ((= n 0) nil) (t (cons v (make-list (- n 1) v)))))
(define frames (make-list NUM_FRAMES 0))  ; 0=free, >0=refcount

(define (alloc-frame)
  (fframe 0))
(define (fframe i)
  (cond ((>= i NUM_FRAMES) -1)     ; out of memory
        ((= (nth i frames) 0) (setq frames (set-nth frames i 1)) i)
        (t (fframe (+ i 1)))))

(define (free-frame idx)
  (setq frames (set-nth frames idx 0)))

(define (ref-frame idx)
  (setq frames (set-nth frames idx (+ (nth idx frames) 1))))

(define (set-nth lst i v)
  (cond ((null lst) nil) ((= i 0) (cons v (cdr lst))) (t (cons (car lst) (set-nth (cdr lst) (- i 1) v)))))

; ---- Page Table Entry: (frame flags) where flags=COW|VALID|DIRTY ----
(define PTE_VALID 1)
(define PTE_DIRTY 2)
(define PTE_COW   4)

(define (make-pte frame flags) (list frame flags))
(define (pte-frame pte) (car pte))
(define (pte-flags pte) (cadr pte))
(define (pte-valid-p pte) (= (logand-vm (pte-flags pte) PTE_VALID) PTE_VALID))
(define (pte-cow-p pte)    (= (logand-vm (pte-flags pte) PTE_COW) PTE_COW))
(define (pte-dirty-p pte)  (= (logand-vm (pte-flags pte) PTE_DIRTY) PTE_DIRTY))

; ---- Process virtual address space ---------------------------
; (pid page-table swap-region)
(define vm-procs nil)

(define (vm-create pid num-pages)
  (let ((pt (make-list num-pages nil)))
    (setq vm-procs (cons (list pid pt nil) vm-procs))
    pid))

(define (vm-get pid) (let ((e (assoc pid vm-procs))) (cond (e (cdr e)) (t nil))))

; Map virtual page -> physical frame
(define (vm-map pid vpage)
  (let ((proc (vm-get pid)))
    (cond ((eq proc nil) (print "[vm] unknown PID") -1)
          (t (let ((pt (car proc)))
               (cond ((>= vpage (length pt)) (print "[vm] page out of bounds") -1)
                     (t (let ((pte (nth vpage pt)))
                          (cond ((eq pte nil) ; page not mapped — allocate
                                 (let ((frame (alloc-frame)))
                                   (cond ((= frame -1) ; OOM — swap out a page
                                          (let ((victim (vm-evict pid)))
                                            (cond ((= victim -1) -1)
                                                  (t (vm-map pid vpage)))))
                                         (t (set-car proc (set-nth pt vpage (make-pte frame PTE_VALID)))
                                            frame))))
                                ((not (pte-valid-p pte)) ; swapped out — page in
                                 (let ((frame (alloc-frame)))
                                   (cond ((= frame -1) -1)
                                         (t (vm-page-in pid vpage frame)
                                            frame))))
                                (t (pte-frame pte)))))))))))

; Handle COW fault: copy the page
(define (vm-cow-fault pid vpage)
  (let ((proc (vm-get pid)))
    (let ((pt (car proc)))
      (let ((pte (nth vpage pt)))
        (cond ((and (not (null pte)) (pte-cow-p pte))
               (let ((old-frame (pte-frame pte))
                     (new-frame (alloc-frame)))
                 (cond ((= new-frame -1) -1)
                       (t (vm-copy-frame old-frame new-frame)
                          (set-car proc (set-nth pt vpage (make-pte new-frame PTE_VALID)))
                          new-frame))))
              (t -1))))))

; Copy-on-write fork
(define (vm-fork parent-pid child-pid)
  (let ((parent (vm-get parent-pid)))
    (let ((pt (car parent)))
      (let ((new-pt (make-list (length pt) nil)))
        ; Mark parent pages COW and copy PTEs to child.  The child is
        ; registered once, by `share`, with the finished page table.
        ; set-nth is pure, so the parent's page table has to be threaded
        ; through the recursion and stored back once at the end —
        ; writing it back each step would keep re-deriving it from the
        ; stale `pt` and only the last page would stay marked.
        (labels (((share i newpt ppt)
            (cond ((>= i (length ppt))
                   (set-car parent ppt)
                   (setq vm-procs (cons (list child-pid newpt nil) vm-procs))
                   child-pid)
                  (t (let ((pte (nth i ppt)))
                       (cond ((not (null pte))
                              (ref-frame (pte-frame pte))
                              (let ((cow (make-pte (pte-frame pte)
                                                   (+ (pte-flags pte) PTE_COW))))
                                (share (+ i 1) (set-nth newpt i cow) (set-nth ppt i cow))))
                             (t (share (+ i 1) newpt ppt))))))))
          (share 0 new-pt pt))))))

; Swap: evict a page to disk
(define (vm-evict pid)
  (let ((proc (vm-get pid)))
    (cond ((eq proc nil) -1)
          (t (let ((pt (car proc)))
               (labels (((find-victim i)
                   (cond ((>= i (length pt)) -1)
                         (t (let ((pte (nth i pt)))
                              (cond ((and (not (null pte)) (pte-valid-p pte) (not (pte-cow-p pte)))
                                     (let ((frame (pte-frame pte)))
                                       (setq swap-disk
                                             (cons (list (list pid i) (swap-out frame)) swap-disk))
                                       (free-frame frame)
                                       (set-car proc (set-nth pt i (make-pte 0 0)))
                                       frame))
                                    (t (find-victim (+ i 1)))))))))
                 (find-victim 0)))))))

(define (swap-out frame) (cons frame nil))  ; placeholder

; Bring a swapped-out page back into `frame`: drop its swap slot and
; re-point the PTE at the new frame, valid again.
; assoc compares keys with eql, which never matches a freshly built
; (pid vpage) list, so look the slot up structurally.
(define (swap-find pid vpage)
  (find (lambda (e) (equal (car e) (list pid vpage))) swap-disk))

(define (vm-page-in pid vpage frame)
  (let ((data (swap-find pid vpage)))
    (cond ((not (null data))
           (setq swap-disk (remove (lambda (x) (equal x data)) swap-disk))))
    (let ((proc (vm-get pid)))
      (set-car proc (set-nth (car proc) vpage (make-pte frame PTE_VALID))))
    frame))

(define (vm-copy-frame src dst) nil)  ; memcpy between frames

; ---- TLB (translation lookaside buffer) ---------------------
(define tlb nil)  ; ((pid vpage frame) ...), newest first, max 8 entries
(define TLB_SIZE 8)

(define (tlb-lookup pid vpage)
  (let ((e (find (lambda (x) (and (= (car x) pid) (= (cadr x) vpage))) tlb)))
    (cond (e (caddr e)) (t nil))))

(define (tlb-insert pid vpage frame)
  ; Entries are newest-first, so the oldest one is the last: drop it.
  (cond ((>= (length tlb) TLB_SIZE) (setq tlb (butlast tlb))))
  (setq tlb (cons (list pid vpage frame) tlb)))

(define (tlb-flush pid) (setq tlb (remove (lambda (x) (= (car x) pid)) tlb)))

; ---- Virtual address translation -----------------------------
(define (vm-translate pid va)
  (let ((vpage (truncate (/ va PAGE_SIZE)))
        (offset (mod va PAGE_SIZE)))
    ; Check TLB first
    (let ((frame (tlb-lookup pid vpage)))
      (cond (frame (+ (* frame PAGE_SIZE) offset))
            (t (let ((f (vm-map pid vpage)))
                 (cond ((= f -1) -1)
                       (t (tlb-insert pid vpage f)
                          (+ (* f PAGE_SIZE) offset)))))))))

; ---- Helpers -------------------------------------------------
(define (logand-vm a b) (logand a b))
(define (logand a b)
  (lacc a b 0 1))
(define (lacc a b pos bit)
  (cond ((= pos 16) 0)
        ((and (= (mod a 2) 1) (= (mod b 2) 1)) (+ bit (lacc (truncate (/ a 2)) (truncate (/ b 2)) (+ pos 1) (* bit 2))))
        (t (lacc (truncate (/ a 2)) (truncate (/ b 2)) (+ pos 1) (* bit 2)))))

(define swap-disk nil)

; ============================================================
; DEMO
; ============================================================
(print "=============================================")
(print "  Virtual Memory — Paging + COW + TLB")
(print "=============================================")
(print "")

(print "--- Page Table Creation ---")
(vm-create 1 8)     ; PID 1, 8 pages (128 words)
(print "PID 1 created with 8 virtual pages")
(print (str-join (list "Physical frames: " (num-to-str NUM_FRAMES) " of " (num-to-str PAGE_SIZE) " words each") ""))
(print "")

(print "--- Page Mapping ---")
(define f0 (vm-map 1 0))  ; map vpage 0
(define f1 (vm-map 1 1))  ; map vpage 1
(print (str-join (list "vpage 0 -> frame " (num-to-str f0)) ""))
(print (str-join (list "vpage 1 -> frame " (num-to-str f1)) ""))
(print (str-join (list "Frames used: " (num-to-str (- NUM_FRAMES (length (filter (lambda (x) (= x 0)) frames))))) ""))
(print "")

(print "--- Address Translation (TLB) ---")
(define pa0 (vm-translate 1 5))    ; va=5 -> vpage=0, offset=5
(define pa1 (vm-translate 1 20))   ; va=20 -> vpage=1, offset=4
(print (str-join (list "VA 5 -> PA " (num-to-str pa0)) ""))
(print (str-join (list "VA 20 -> PA " (num-to-str pa1)) ""))
(print (str-join (list "TLB entries: " (num-to-str (length tlb))) ""))
(print "")

(print "--- Copy-on-Write Fork ---")
(vm-fork 1 2)
(print "PID 2 forked from PID 1 (COW pages shared)")
(print (str-join (list "Frames used: " (num-to-str (- NUM_FRAMES (length (filter (lambda (x) (= x 0)) frames))))) ""))
(print "")

(print "--- Page Fault / Swap ---")
(define f2 (vm-map 1 2))
(define f3 (vm-map 1 3))
(define f4 (vm-map 1 4))
(define f5 (vm-map 1 5))
(print "Mapped pages 0-5, approaching frame limit")
(print (str-join (list "Frames used: " (num-to-str (- NUM_FRAMES (length (filter (lambda (x) (= x 0)) frames))))) ""))
(print "")

(print "--- Checks ---")

(define (pt-of pid) (car (vm-get pid)))

(check "distinct virtual pages get distinct frames"
       (and (= f0 0) (and (= f1 1) (not (= f0 f1)))))
(check "re-mapping a resident page returns the same frame"
       (and (= (vm-map 1 0) f0) (= (vm-map 1 1) f1)))
(check "translation is frame*PAGE_SIZE + offset"
       (and (= pa0 5) (= pa1 20)))
(check "TLB caches both translations" (= (length tlb) 2))
(check "TLB lookup returns the frame number, not a list"
       (= (tlb-lookup 1 1) f1))
(check "TLB miss for a page never translated" (null (tlb-lookup 1 4)))

; COW fork: parent and child share frames, both sides marked COW.
(check "fork registers the child exactly once"
       (= (count (lambda (e) (= (car e) 2)) vm-procs) 1))
(check "fork shares the parent's frames with the child"
       (and (= (pte-frame (nth 0 (pt-of 2))) f0)
            (= (pte-frame (nth 1 (pt-of 2))) f1)))
(check "fork marks both parent and child pages copy-on-write"
       (and (pte-cow-p (nth 0 (pt-of 1))) (pte-cow-p (nth 0 (pt-of 2)))))
(check "fork bumps the shared frames' refcounts" (= (nth f0 frames) 2))

; A write by the child triggers the copy.
(define cow-frame (vm-cow-fault 2 0))
(check "COW fault allocates a fresh frame for the writer"
       (and (> cow-frame 0) (not (= cow-frame f0))))
(check "COW fault leaves the child's page private and valid"
       (and (= (pte-frame (nth 0 (pt-of 2))) cow-frame)
            (and (pte-valid-p (nth 0 (pt-of 2)))
                 (not (pte-cow-p (nth 0 (pt-of 2)))))))
(check "COW fault does not disturb the parent's mapping"
       (= (pte-frame (nth 0 (pt-of 1))) f0))
(check "a page that is not COW cannot fault this way"
       (= (vm-cow-fault 1 2) -1))

; Eviction to swap, then a page-in on the next touch.
(define swap-before (length swap-disk))
(define victim (vm-evict 1))
(check "eviction picks a resident non-COW page"
       (and (= victim 2) (= (nth 2 frames) 0)))
(check "eviction records the page on the swap disk"
       (= (length swap-disk) (+ swap-before 1)))
(check "the evicted page is no longer valid"
       (not (pte-valid-p (nth 2 (pt-of 1)))))
(define reloaded (vm-map 1 2))
(check "touching a swapped page pages it back in"
       (and (= reloaded 2) (pte-valid-p (nth 2 (pt-of 1)))))
(check "paging in clears the swap slot" (= (length swap-disk) swap-before))

; TLB replacement.
(tlb-flush 1) (tlb-flush 2)
(check "flushing empties the TLB" (= (length tlb) 0))
(foreach (lambda (i) (tlb-insert 1 i i)) (list 0 1 2 3 4 5 6 7 8 9))
(check "TLB never exceeds TLB_SIZE" (= (length tlb) TLB_SIZE))
(check "TLB evicts the oldest entries first"
       (and (null (tlb-lookup 1 0)) (= (tlb-lookup 1 9) 9)))
(print "")

(print "=============================================")
(print "  VM — Paging, COW, TLB all verified")
(print "=============================================")

