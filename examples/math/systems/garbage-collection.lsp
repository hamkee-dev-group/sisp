; ============================================================
; 84 — Garbage Collection (TAOCP Vol 1, 2.3.5)
; Mark-and-sweep over a simulated heap with cycles, then a
; copying collector with from-space, to-space, forward pointers.
; ============================================================

(print "=== Garbage Collection ===")

(define (test name ok)
  (cond (ok (print (list (quote pass) name)))
        (t (print (list (quote FAIL) name)))))

; ---------- part 1: mark-and-sweep ----------
; heap entry: (id marked reachable-from ...) where id is a symbol
; reachable entries have id as first element of reachable-from list

(define (heap-entry id marked refs) (cons id (cons marked refs)))
(define (ent-id e) (car e))
(define (ent-marked e) (cadr e))
(define (ent-refs e) (cddr e))
(define (ent-set-marked e v) (set-car (cdr e) v))

; mark phase: depth-first from roots, sets marked = t
(define (mark-from entry heap)
  (cond ((ent-marked entry) nil)
        (t (progn (ent-set-marked entry t)
           (mark-refs (ent-refs entry) heap)
           nil))))

(define (mark-refs refs heap)
  (cond ((eq refs nil) nil)
        (t (let ((target (find-entry (car refs) heap)))
          (mark-from target heap)
          (mark-refs (cdr refs) heap)))))

(define (find-entry id heap)
  (cond ((eq heap nil) nil)
        ((eq (ent-id (car heap)) id) (car heap))
        (t (find-entry id (cdr heap)))))

; sweep: collect unmarked entries
(define (sweep heap acc)
  (cond ((eq heap nil) (reverse-acc acc nil))
        ((ent-marked (car heap)) (sweep (cdr heap) (cons (car heap) acc)))
        (t (sweep (cdr heap) acc))))

(define (reverse-acc lst acc)
  (cond ((eq lst nil) acc)
        (t (reverse-acc (cdr lst) (cons (car lst) acc)))))

; build a graph with a cycle: a -> b -> c -> a, plus a dangling d
; a refers to b, b refers to c, c refers to a; d is unreferenced
(define (build-mark-sweep-heap)
  (let* ((a (heap-entry (quote a) nil (list (quote b))))
         (b (heap-entry (quote b) nil (list (quote c))))
         (c (heap-entry (quote c) nil (list (quote a))))
         (d (heap-entry (quote d) nil nil))
         (heap (list a b c d)))
    heap))

(define ms-heap (build-mark-sweep-heap))
(print (list (quote initial-heap-ids) (map ent-id ms-heap)))

; mark from root a; a -> b -> c -> (already marked a, stop)
(mark-from (find-entry (quote a) ms-heap) ms-heap)

(define ms-after (sweep ms-heap nil))
(print (list (quote after-mark-sweep-ids) (map ent-id ms-after)))

(test "mark-sweep-cycle-kept-a-b-c" (and (in (quote a) (list-to-set (map ent-id ms-after)))
                                          (in (quote b) (list-to-set (map ent-id ms-after)))
                                          (in (quote c) (list-to-set (map ent-id ms-after)))))
(test "mark-sweep-unreferenced-d-removed" (not (in (quote d) (list-to-set (map ent-id ms-after)))))
(test "mark-sweep-survived-count-3" (= (length ms-after) 3))

; ---------- part 2: copying collector ----------
; semispace: list of slots. Objects as (id field1 field2 ...)
; forwarding: a marked/fwd pointer replaces the object

(define (copy-obj id fields) (cons id fields))
(define (cop-id obj) (car obj))
(define (cop-fields obj) (cdr obj))
(define (cop-field obj n)
  (cond ((= n 0) (cadr obj))
        (t (cop-field (cdr obj) (- n 1)))))
(define (cop-set-field obj n v)
  (cond ((= n 0) (set-car (cdr obj) v))
        (t (cop-set-field (cdr obj) (- n 1) v))))

; forwarding indicator: we reuse the object slot
; after forwarding, car of the original slot holds (quote fwd) and cdr is the new address
(define (fwd-p x) (and (consp x) (eq (car x) (quote fwd))))
(define (fwd-target x) (cdr x))
(define (make-fwd target) (cons (quote fwd) target))

; roots: list of from-space indices
; objects refer to each other by index in from-space
; build a small graph in from-space and evacuate it
(define from-space nil)
(define to-space nil)
(define to-free 0)

(define (init-copying-collector)
  (setq from-space (list (copy-obj (quote r) (list 1))
                         (copy-obj (quote a) (list 2))
                         (copy-obj (quote b) (list 0))
                         (copy-obj (quote x) nil)))
  (setq to-space (list nil nil nil nil nil nil nil nil))
  (setq to-free 0))

; evacuate object at from-index to to-space, return to-space index
(define (evacuate fi)
  (let ((obj (nth fi from-space)))
    (cond ((fwd-p obj) (fwd-target obj))
          (t (let ((new-idx to-free))
            (setq to-free (+ to-free 1))
            (progn (set-car (nthcdr new-idx to-space) (cons (cop-id obj) nil))
              (set-car (nthcdr fi from-space) (make-fwd new-idx))
              (evac-fields (cop-fields obj) new-idx)
              new-idx))))))

(define (nthcdr n lst)
  (cond ((= n 0) lst)
        (t (nthcdr (- n 1) (cdr lst)))))

(define (evac-fields fields ti)
  (cond ((eq fields nil) nil)
        ((numberp (car fields))
         (let ((fwd-idx (evacuate (car fields))))
           (set-car (nthcdr ti to-space)
             (append (car (nthcdr ti to-space)) (list fwd-idx)))
           nil))
        (t nil)))

; scavenge: process to-space objects until done
(define (scavenge scan-idx)
  (cond ((= scan-idx to-free) nil)
        (t (let ((obj (car (nthcdr scan-idx to-space))))
          (evac-fields (cdr obj) scan-idx)
          (scavenge (+ scan-idx 1))))))

(init-copying-collector)
; root is at index 0
(evacuate 0)
(scavenge 0)

(define survivor-ids (map cop-id (take to-space to-free)))
(print (list (quote copying-survivors) survivor-ids))

(test "copying-collector-preserves-cycle" (and (in (quote r) (list-to-set survivor-ids))
                                                (in (quote a) (list-to-set survivor-ids))
                                                (in (quote b) (list-to-set survivor-ids))))
(test "copying-collector-drops-unreferenced-x" (not (in (quote x) (list-to-set survivor-ids))))
(test "copying-collector-survivor-count" (= (length survivor-ids) 3))

(print "=== Garbage Collection Done ===")
