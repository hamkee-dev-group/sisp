; ============================================================
; 101 — Reduced Ordered BDDs (TAOCP Vol 4A, 7.1.4)
; Build ROBDDs for Boolean functions of 3 variables.
; Unique table hash by (variable, low, high) for node sharing.
; Reduction: remove duplicate nodes, remove redundant nodes.
; Functions: f1 = x1 AND x2, f2 = x1 OR x2, f3 = (x1 AND x2) OR x3.
; Apply AND/OR/NOT on BDDs. Verify truth tables match.
; ============================================================

(print "=== ROBDDs ===")


; ---------- truth vectors ----------
; Standard ordering: f(0,0,0) f(0,0,1) f(0,1,0) f(0,1,1) f(1,0,0) f(1,0,1) f(1,1,0) f(1,1,1)

(define (tv-row i)
  (let* ((x3 (not (= (mod i 2) 0)))
         (x2 (not (= (mod (truncate (/ i 2)) 2) 0)))
         (x1 (not (= (mod (truncate (/ i 4)) 2) 0))))
    (list x1 x2 x3)))

(define (truth-table f)
  (labels (((collect i acc)
    (cond ((< i 0) acc)
          (t (collect (- i 1) (cons (f (tv-row i)) acc))))))
    (collect 7 nil)))

(define (f1-eval a) (and (car a) (cadr a)))
(define (f2-eval a) (or (car a) (cadr a)))
(define (f3-eval a) (or (and (car a) (cadr a)) (caddr a)))

(define tv-f1 (truth-table f1-eval))
(define tv-f2 (truth-table f2-eval))
(define tv-f3 (truth-table f3-eval))

(print (list (quote tv-f1) tv-f1))
(print (list (quote tv-f2) tv-f2))
(print (list (quote tv-f3) tv-f3))

; ---------- BDD representation ----------
; A "BDD node" is a list (var low high). Terminal 0 and 1 are atoms.
; The unique table is an association list mapping (var low high) -> canonical node.
; We represent nodes as their integer ID (0, 1, 2, ...).

(define *ut* nil)       ; ((var low high) . id)
(define *next-id* 2)
(define *node-info* nil) ; (id . (var low high))

(define (ut-get var lo hi)
  (labels (((search entries)
    (cond ((eq entries nil) nil)
          ((and (= (caar entries) var)
                (= (cadr (car entries)) lo)
                (= (caddr (car entries)) hi))
           (cadddr (car entries)))
          (t (search (cdr entries))))))
    (search *ut*)))

(define (ut-put var lo hi id)
  (setq *ut* (cons (list var lo hi id) *ut*))
  id)

(define (node-var id)
  (cond ((eq id 0) 0)
        ((eq id 1) 0)
        (t (let ((e (assoc id *node-info*)))
          (cond (e (cadr e)) (t 0))))))

(define (node-low id)
  (cond ((eq id 0) 0)
        ((eq id 1) 1)
        (t (let ((e (assoc id *node-info*)))
          (cond (e (caddr e)) (t 0))))))

(define (node-high id)
  (cond ((eq id 0) 0)
        ((eq id 1) 1)
        (t (let ((e (assoc id *node-info*)))
          (cond (e (cadddr e)) (t 0))))))

(define (mk-node var lo hi)
  (cond ((= lo hi) lo)
        (t (let ((existing (ut-get var lo hi)))
          (cond (existing existing)
                (t (let ((id *next-id*))
                  (progn
                    (setq *next-id* (+ *next-id* 1))
                    (setq *node-info* (cons (list id var lo hi) *node-info*))
                    (ut-put var lo hi id)
                    id))))))))

; ---------- evaluate BDD ----------
(define (bdd-eval node assign)
  (cond ((eq node 0) nil)
        ((eq node 1) t)
        (t (let ((var (node-var node)))
          (cond ((nth (- var 1) assign)
                 (bdd-eval (node-high node) assign))
                (t (bdd-eval (node-low node) assign)))))))

; ---------- build BDD from truth vector bottom-up ----------
(define (build-bdd tv vars)
  (let ((len-vars (length vars)))
    (cond ((= len-vars 0) (cond ((car tv) 1) (t 0)))
          (t (let* ((v (car vars))
                    (half (^ 2 (- len-vars 1)))
                    (lo (build-bdd (take tv half) (cdr vars)))
                    (hi (build-bdd (drop tv half) (cdr vars))))
            (mk-node v lo hi))))))

; Build the three BDDs
(define bdd-f1 (build-bdd tv-f1 (list 1 2 3)))
(define bdd-f2 (build-bdd tv-f2 (list 1 2 3)))
(define bdd-f3 (build-bdd tv-f3 (list 1 2 3)))

(print (list (quote bdd-f1) bdd-f1))
(print (list (quote bdd-f2) bdd-f2))
(print (list (quote bdd-f3) bdd-f3))
(print (list (quote ut-size) (length *ut*)))

; ---------- verify truth tables ----------
(define (bdd-truth-table node)
  (labels (((collect i acc)
    (cond ((< i 0) acc)
          (t (collect (- i 1) (cons (bdd-eval node (tv-row i)) acc))))))
    (collect 7 nil)))

(check "f1-truth-table-matches"
  (equal (bdd-truth-table bdd-f1) tv-f1))
(check "f2-truth-table-matches"
  (equal (bdd-truth-table bdd-f2) tv-f2))
(check "f3-truth-table-matches"
  (equal (bdd-truth-table bdd-f3) tv-f3))

; ---------- BDD operations: AND, OR, NOT ----------
; Memoized apply with cache
(define *apply-cache* nil)

(define (app-put op a b result)
  (setq *apply-cache* (cons (list op a b result) *apply-cache*))
  result)

(define (app-get op a b)
  (labels (((search entries)
    (cond ((eq entries nil) nil)
          ((and (eq (car entries) op)
                (= (cadr entries) a)
                (= (caddr entries) b))
           (cadddr entries))
          (t (search (cdr entries))))))
    (search *apply-cache*)))

(define (is-terminal x) (or (= x 0) (= x 1)))

(define (app op a b)
  (let ((cached (app-get op a b)))
    (cond (cached cached)
          ((and (is-terminal a) (is-terminal b))
           (app-put op a b (cond ((op a b) 1) (t 0))))
          ((is-terminal a) (app op b a))
          ((is-terminal b)
           (let ((va (node-var a))
                 (vlo (app op (node-low a) b))
                 (vhi (app op (node-high a) b)))
             (app-put op a b (mk-node va vlo vhi))))
          ((< (node-var a) (node-var b))
           (let ((va (node-var a))
                 (vlo (app op (node-low a) b))
                 (vhi (app op (node-high a) b)))
             (app-put op a b (mk-node va vlo vhi))))
          ((> (node-var a) (node-var b))
           (app op b a))
          (t (let ((va (node-var a))
                   (vlo (app op (node-low a) (node-low b)))
                   (vhi (app op (node-high a) (node-high b))))
             (app-put op a b (mk-node va vlo vhi)))))))

(define (bdd-and a b)
  (app (lambda (x y) (and (= x 1) (= y 1))) a b))

(define (bdd-or a b)
  (app (lambda (x y) (or (= x 1) (= y 1))) a b))

(define (bdd-neg a)
  (app (lambda (x y) (not (= x 1))) a a))

; ---------- test operations ----------
(define bdd-f1-f2 (bdd-and bdd-f1 bdd-f2))
(define bdd-f1+f2 (bdd-or bdd-f1 bdd-f2))
(define bdd-nf1 (bdd-neg bdd-f1))

(print (list (quote f1-and-f2) bdd-f1-f2))
(print (list (quote f1-or-f2) bdd-f1+f2))
(print (list (quote not-f1) bdd-nf1))

(check "and-matches-truth-table"
  (equal (bdd-truth-table bdd-f1-f2)
         (map (lambda (pair) (and (car pair) (cadr pair)))
              (zip tv-f1 tv-f2))))
(check "or-matches-truth-table"
  (equal (bdd-truth-table bdd-f1+f2)
         (map (lambda (pair) (or (car pair) (cadr pair)))
              (zip tv-f1 tv-f2))))
(check "not-matches-truth-table"
  (equal (bdd-truth-table bdd-nf1)
         (map not tv-f1)))
(check "f1-and-f2-equals-f1"
  (equal (bdd-truth-table bdd-f1-f2) tv-f1))
(check "unique-table-nonempty" (> (length *ut*) 0))

(print "=== ROBDDs Done ===")
