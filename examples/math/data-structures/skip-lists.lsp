; ============================================================
; name: skip-list
; subject: data-structure
; capability: probabilistic sorted map
; status: incomplete
; canonical-home: none
; notes: Imperative/probabilistic; a node allocates one more forward slot
;        than its level uses (harmless).
; ============================================================
; ============================================================
; 125 — Skip Lists (Pugh 1990)
; Probabilistic ordered structure with multiple levels of
; linked lists. Nodes carry a value and forward pointers at
; each level. Insert, search, delete. Level chosen by seeded
; coin flips for deterministic reproducibility.
; Verify: all elements found after insertion, sorted order
; maintained. Test with 20 deterministic random insertions.
; ============================================================

(print "=== Skip Lists (Pugh 1990) ===")


(define (list-with xs i v)
  (cond ((= i 0) (cons v (cdr xs)))
        (t (cons (car xs) (list-with (cdr xs) (- i 1) v)))))

(define max-lvl 10)

; node = (val forward-list)  forward-list has max-lvl entries (nil or next node)
(define (sl-v n) (car n))
(define (sl-fwds n) (cadr n))
(define (sl-fwd n lv) (nth lv (sl-fwds n)))
(define (sl-setf n fwds) (set-car (cdr n) fwds))
(define (sl-setfwd n lv v)
  (sl-setf n (list-with (sl-fwds n) lv v)))

(define (mk-sl-node val lv)
  (list val (map (lambda (x) nil) (seq 1 (+ lv 1)))))

(define hdr (mk-sl-node (quote hdr) max-lvl))

; ---------- random level (seeded coin flips) ----------
(define (rand-lvl) (rand-lvl-aux 1))
(define (rand-lvl-aux cur)
  (cond ((>= cur max-lvl) max-lvl)
        ((< (random 2) 1) cur)
        (t (rand-lvl-aux (+ cur 1)))))

; ---------- find predecessor at each level, store in *update* ----------
(setq *update* (map (lambda (x) nil) (seq 1 (+ max-lvl 1))))

(define (build-update val lv)
  (cond ((< lv 0) nil)
        (t (progn
             (setq *update* (list-with *update* lv (find-pred val lv)))
             (build-update val (- lv 1))))))

(define (find-pred val lv)
  (cond ((eq lv (- max-lvl 1)) (find-pred-from hdr val lv))
        (t (find-pred-from (nth (+ lv 1) *update*) val lv))))

(define (find-pred-from cur val lv)
  (cond ((and (not (eq cur nil))
              (not (eq (sl-fwd cur lv) nil))
              (< (sl-v (sl-fwd cur lv)) val))
         (find-pred-from (sl-fwd cur lv) val lv))
        (t cur)))

; ---------- insert ----------
(define (sl-insert val)
  (let* ((lvl (rand-lvl))
         (n (mk-sl-node val lvl)))
    (build-update val (- max-lvl 1))
    (link-insert n lvl (- lvl 1))))

(define (link-insert new-nd lvl-max lv)
  (cond ((< lv 0) nil)
        (t (let ((pred (nth lv *update*)))
             (sl-setfwd new-nd lv (sl-fwd pred lv))
             (sl-setfwd pred lv new-nd)
             (link-insert new-nd lvl-max (- lv 1))))))

; ---------- search ----------
(define (sl-find val)
  (let ((nd (search-from hdr (- max-lvl 1) val)))
    (cond ((eq nd nil) nil)
          ((= (sl-v nd) val) nd)
          (t nil))))

(define (sl-has val)
  (not (eq (sl-find val) nil)))

(define (search-from cur lv val)
  (cond ((< lv 0) (cond ((not (eq cur nil)) (sl-fwd cur 0))
                        (t nil)))
        ((and (not (eq cur nil))
              (not (eq (sl-fwd cur lv) nil))
              (< (sl-v (sl-fwd cur lv)) val))
         (search-from (sl-fwd cur lv) lv val))
        (t (search-from cur (- lv 1) val))))

; ---------- delete ----------
(define (sl-delete val)
  (build-update val (- max-lvl 1))
  (let ((target (sl-fwd (nth 0 *update*) 0)))
    (cond ((or (eq target nil) (not (= (sl-v target) val))) nil)
          (t (del-from-update-target target (- max-lvl 1))))))

(define (del-from-update-target target lv)
  (cond ((< lv 0) nil)
        ((< lv (length (sl-fwds target)))
         (let ((pred (nth lv *update*)))
           (cond ((and (not (eq pred nil))
                       (eq (sl-fwd pred lv) target))
                  (sl-setfwd pred lv (sl-fwd target lv)))
                 (t nil))
           (del-from-update-target target (- lv 1))))
        (t (del-from-update-target target (- lv 1)))))

; ---------- flatten to list ----------
(define (sl-flatten)
  (sl-walk (sl-fwd hdr 0) nil))

(define (sl-walk nd acc)
  (cond ((eq nd nil) (reverse acc))
        (t (sl-walk (sl-fwd nd 0) (cons (sl-v nd) acc)))))

; ---------- sorted check ----------
(define (sortedp lst)
  (cond ((or (eq lst nil) (eq (cdr lst) nil)) t)
        ((> (car lst) (cadr lst)) nil)
        (t (sortedp (cdr lst)))))

; ---------- reset ----------
(define (reset-sl)
  (setq hdr (mk-sl-node (quote hdr) max-lvl)))

; ========== tests ==========
(reset-sl)
(seed 42)

(check "empty-flatten-nil" (eq (sl-flatten) nil))
(check "empty-search-nil" (eq (sl-has 42) nil))

(sl-insert 30)
(sl-insert 10)
(sl-insert 50)
(sl-insert 20)
(sl-insert 40)

(print (list (quote after-5-inserts) (sl-flatten)))

(check "found-10" (sl-has 10))
(check "found-20" (sl-has 20))
(check "found-30" (sl-has 30))
(check "found-40" (sl-has 40))
(check "found-50" (sl-has 50))
(check "not-found-99" (eq (sl-has 99) nil))
(check "not-found-0" (eq (sl-has 0) nil))
(check "sorted-after-5-inserts" (sortedp (sl-flatten)))

(sl-delete 30)
(print (list (quote after-delete-30) (sl-flatten)))
(check "not-found-30-after-delete" (eq (sl-has 30) nil))
(check "others-still-found-after-delete"
  (and (sl-has 10) (sl-has 20) (sl-has 40) (sl-has 50)))
(check "sorted-after-delete" (sortedp (sl-flatten)))

(sl-delete 10)
(sl-delete 50)
(print (list (quote after-delete-10-50) (sl-flatten)))
(check "only-20-40-remain" (equal (sl-flatten) (list 20 40)))
(check "sorted-after-more-deletes" (sortedp (sl-flatten)))

; ---------- 20 deterministic random insertions ----------
(print "--- 20 random insertions, seeded ---")
(reset-sl)
(seed 12345)

(define rvals nil)
(setq rvals nil)

(define (batch-insert i)
  (cond ((= i 20) nil)
        (t (let ((v (random 1000)))
             (setq rvals (cons v rvals))
             (sl-insert v)
             (batch-insert (+ i 1))))))
(batch-insert 0)
(setq rvals (reverse rvals))

(print (list (quote inserted-values) rvals))
(print (list (quote flattened-skip-list) (sl-flatten)))

(check "all-20-found-by-search"
  (every (lambda (v) (sl-has v)) rvals))
(check "sorted-after-20-random-inserts"
  (sortedp (sl-flatten)))
(check "flatten-has-20-elements"
  (= (length (sl-flatten)) 20))
(check "all-flattened-searchable"
  (every (lambda (v) (sl-has v)) (sl-flatten)))

; random lookup not in set
(check "absent-value-not-found" (eq (sl-has 9999) nil))

(print "=== Skip Lists Done ===")
