; ============================================================ ; 109 — Generic Backtracking (TAOCP Vol 4A, 7.2.2) ; Reusable search engine with explicit choose/test/undo state. ; (a) Knight's tour on 3x3 board — no tours, verify count=0. ; (b) Langford pairing L(3,3) — arrangement (2,3,1,2,1,3). ; ============================================================
(print "=== Generic Backtracking ===")


; ---------- generic backtracking ----------
(define (backtrack choose apl found state depth max-depth)
  (cond ((found state) 1)
        ((= depth max-depth) 0)
        (t (let ((cands (choose state)))
          (foldl (lambda (acc move)
            (+ acc (backtrack choose apl found
                     (apl state move) (+ depth 1) max-depth)))
            0
            cands)))))

; ============================================================ ; (a) Knight's Tour on 3x3 board ; ============================================================
(print "--- Knight's Tour on 3x3 board ---")

(define kmoves
  (list (cons 2 1) (cons 2 -1) (cons -2 1) (cons -2 -1)
        (cons 1 2) (cons 1 -2) (cons -1 2) (cons -1 -2)))

(define (kchoose s)
  (let ((x (car s)) (y (cadr s)) (v (caddr s)))
    (filter (lambda (mv)
      (let ((nx (+ x (car mv))) (ny (+ y (cdr mv))))
        (and (>= nx 0) (< nx 3) (>= ny 0) (< ny 3)
             (not (in (list nx ny) v)))))
      kmoves)))

(define (kapply s mv)
  (let ((x (car s)) (y (cadr s)) (v (caddr s)))
    (let ((nx (+ x (car mv))) (ny (+ y (cdr mv))))
      (list nx ny (union v (list-to-set (list (list nx ny))))))))

(define (kfound s)
  (= (length (caddr s)) 9))

(define (ktours sx sy)
  (backtrack kchoose kapply kfound
    (list sx sy (list-to-set (list (list sx sy)))) 1 9))

(define ktotal 0)
(setq ktotal (+ ktotal (ktours 0 0)))
(setq ktotal (+ ktotal (ktours 0 1)))
(setq ktotal (+ ktotal (ktours 0 2)))
(setq ktotal (+ ktotal (ktours 1 0)))
(setq ktotal (+ ktotal (ktours 1 1)))
(setq ktotal (+ ktotal (ktours 1 2)))
(setq ktotal (+ ktotal (ktours 2 0)))
(setq ktotal (+ ktotal (ktours 2 1)))
(setq ktotal (+ ktotal (ktours 2 2)))
(print "Total knight tours on 3x3:")
(print ktotal)
(check "knight-3x3-no-tours" (= ktotal 0))

; ============================================================ ; (b) Langford pairing L(3,3) ; ============================================================
(print "--- Langford pairing L(3,3) ---")

; State: (slots remaining) where slots is list-of-6 and remaining is list of numbers yet to place

(define (lang-first-empty slots i)
  (cond ((= i 6) -1)
        ((eq (nth i slots) nil) i)
        (t (lang-first-empty slots (+ i 1)))))

(define (lang-set lst idx val)
  (cond ((= idx 0) (cons val (cdr lst)))
        (t (cons (car lst) (lang-set (cdr lst) (- idx 1) val)))))

(define (lang-remove x lst)
  (cond ((eq lst nil) nil)
        ((= (car lst) x) (cdr lst))
        (t (cons (car lst) (lang-remove x (cdr lst))))))

(define (lang-choose state)
  (let ((slots (car state)) (remaining (cadr state)))
    (cond ((eq remaining nil) nil)
          (t (let ((ff (lang-first-empty slots 0)))
            (cond ((= ff -1) nil)
                  (t (map (lambda (k) (list ff k))
                     (filter (lambda (k)
                       (let ((pos2 (+ ff k 1)))
                         (and (< pos2 6)
                              (eq (nth pos2 slots) nil))))
                       remaining)))))))))

(define (lang-apply state move)
  (let ((slots (car state)) (remaining (cadr state))
        (pos (car move)) (num (cadr move)))
    (let ((pos2 (+ pos num 1)))
      (list (lang-set (lang-set slots pos num) pos2 num)
            (lang-remove num remaining)))))

(define (lang-found state)
  (eq (cadr state) nil))

(define lang-count
  (backtrack lang-choose lang-apply lang-found
    (list (list nil nil nil nil nil nil) (list 1 2 3)) 0 3))

(print "Langford L(3,3) solutions:")
(print lang-count)
(check "langford-has-solutions" (> lang-count 0))

; Known solution: (2 3 1 2 1 3)
(define known (list 2 3 1 2 1 3))

(define (find-positions sol target i acc)
  (cond ((= i 6) (reverse acc))
        ((= (nth i sol) target)
         (find-positions sol target (+ i 1) (cons i acc)))
        (t (find-positions sol target (+ i 1) acc))))

(define (verify-lang sol)
  (let ((pos1 (find-positions sol 1 0 nil))
        (pos2 (find-positions sol 2 0 nil))
        (pos3 (find-positions sol 3 0 nil)))
    (and (= (length pos1) 2)
         (= (length pos2) 2)
         (= (length pos3) 2)
         (= (- (cadr pos1) (car pos1)) 2)
         (= (- (cadr pos2) (car pos2)) 3)
         (= (- (cadr pos3) (car pos3)) 4))))

(check "known-langford-valid" (verify-lang known))

; Collect all solutions and verify
(define (lang-collect choose ap found state depth maxd)
  (cond ((found state) (list (car state)))
        ((= depth maxd) nil)
        (t (let ((cands (choose state)))
          (foldl (lambda (acc move)
            (append acc
              (lang-collect choose ap found
                (ap state move) (+ depth 1) maxd)))
            nil cands)))))

(define all-lsols
  (lang-collect lang-choose lang-apply lang-found
    (list (list nil nil nil nil nil nil) (list 1 2 3)) 0 3))

(check "all-langford-solutions-valid"
  (foldl (lambda (acc s) (and acc (verify-lang s))) t all-lsols))

(print "=== Generic Backtracking Done ===")
