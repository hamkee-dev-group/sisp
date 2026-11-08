; ============================================================
; 82 - Buddy-System Storage Allocation (TAOCP Vol 1, 2.5)
; A 64-word heap is a list of ordered blocks (start size state id).
; Allocation splits powers of two; release repeatedly coalesces
; free buddies. Layout and coalescing invariants are checked.
; ============================================================

(print "=== Buddy-System Storage Allocation ===")


(define (block start size state id) (list start size state id))
(define (block-start value) (car value))
(define (block-size value) (cadr value))
(define (block-state value) (caddr value))
(define (block-id value) (cadddr value))
(define (block-free-p value) (eq (block-state value) 'free))

(define (power-of-two-at-least request size)
  (cond ((>= size request) size)
        (t (power-of-two-at-least request (* size 2)))))

; Split only the left descendant until it has the requested size.
(define (buddy-split-allocation start size target id)
  (cond ((= size target) (list (block start size 'used id)))
        (t (let ((half (truncate (/ size 2))))
          (append (buddy-split-allocation start half target id)
            (list (block (+ start half) half 'free nil)))))))

(define (buddy-allocate-size heap target id)
  (cond ((eq heap nil) nil)
        ((and (block-free-p (car heap))
              (>= (block-size (car heap)) target))
         (append
           (buddy-split-allocation (block-start (car heap))
             (block-size (car heap)) target id)
           (cdr heap)))
        (t (cons (car heap)
             (buddy-allocate-size (cdr heap) target id)))))

(define (buddy-allocate heap request id)
  (let ((target (power-of-two-at-least request 1)))
    (cond ((> target 64) heap)
          (t (buddy-allocate-size heap target id)))))

(define (buddy-release-mark heap id)
  (map (lambda (value)
    (cond ((and (not (block-free-p value)) (eq (block-id value) id))
           (block (block-start value) (block-size value) 'free nil))
          (t value))) heap))

(define (buddy-pair-p left right)
  (and (block-free-p left) (block-free-p right)
       (= (block-size left) (block-size right))
       (= (block-start right) (+ (block-start left) (block-size left)))
       (= (mod (block-start left) (* 2 (block-size left))) 0)))

(define (buddy-coalesce-pass heap)
  (cond ((or (eq heap nil) (eq (cdr heap) nil)) heap)
        ((buddy-pair-p (car heap) (cadr heap))
         (cons (block (block-start (car heap))
                 (* 2 (block-size (car heap))) 'free nil)
           (buddy-coalesce-pass (cdr (cdr heap)))))
        (t (cons (car heap) (buddy-coalesce-pass (cdr heap))))))

(define (buddy-coalesce heap)
  (let ((next (buddy-coalesce-pass heap)))
    (cond ((equal next heap) heap) (t (buddy-coalesce next)))))

(define (buddy-release heap id)
  (buddy-coalesce (buddy-release-mark heap id)))

(define (buddy-find heap id)
  (find (lambda (value)
    (and (not (block-free-p value)) (eq (block-id value) id))) heap))

(define (power-of-two-p n)
  (cond ((= n 1) t) ((or (< n 1) (oddp n)) nil)
        (t (power-of-two-p (truncate (/ n 2))))))

(define (buddy-layout-walk heap expected total)
  (cond ((eq heap nil) (= expected total))
        (t (let ((value (car heap)))
          (and (= (block-start value) expected)
               (power-of-two-p (block-size value))
               (= (mod (block-start value) (block-size value)) 0)
               (buddy-layout-walk (cdr heap)
                 (+ expected (block-size value)) total))))))

(define (buddy-layout-p heap total)
  (buddy-layout-walk heap 0 total))

(define (buddy-fully-coalesced-p heap)
  (cond ((or (eq heap nil) (eq (cdr heap) nil)) t)
        ((buddy-pair-p (car heap) (cadr heap)) nil)
        (t (buddy-fully-coalesced-p (cdr heap)))))

(define heap0 (list (block 0 64 'free nil)))
(define heap1 (buddy-allocate heap0 5 'alpha))
(define heap2 (buddy-allocate heap1 9 'beta))
(define heap3 (buddy-allocate heap2 7 'gamma))
(print (list 'allocated-heap heap3))

(check "requests-round-to-eight-sixteen-eight"
  (and (= (block-size (buddy-find heap3 'alpha)) 8)
       (= (block-size (buddy-find heap3 'beta)) 16)
       (= (block-size (buddy-find heap3 'gamma)) 8)))
(check "allocated-starts-are-zero-sixteen-eight"
  (and (= (block-start (buddy-find heap3 'alpha)) 0)
       (= (block-start (buddy-find heap3 'beta)) 16)
       (= (block-start (buddy-find heap3 'gamma)) 8)))
(check "split-layout-covers-heap" (buddy-layout-p heap3 64))
(check "split-layout-has-no-mergeable-free-buddies"
  (buddy-fully-coalesced-p heap3))

; Freeing alpha alone cannot merge because gamma is its buddy.
(define heap4 (buddy-release heap3 'alpha))
(check "occupied-buddy-prevents-merge" (= (block-size (car heap4)) 8))

; Free gamma: the two 8s merge to 16, but beta still blocks 32.
(define heap5 (buddy-release heap4 'gamma))
(check "released-eight-word-buddies-merge"
  (and (= (block-size (car heap5)) 16) (block-free-p (car heap5))))

; Free beta: 16+16 -> 32, then 32+32 -> the original 64-word block.
(define heap6 (buddy-release heap5 'beta))
(print (list 'fully-released-heap heap6))
(check "all-releases-reconstruct-original-heap" (equal heap6 heap0))
(check "final-layout-valid" (buddy-layout-p heap6 64))
(check "oversized-request-leaves-heap-unchanged"
  (equal (buddy-allocate heap0 65 'too-large) heap0))

(print "=== Buddy-System Storage Allocation Done ===")
