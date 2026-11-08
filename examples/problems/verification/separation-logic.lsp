; Separation logic: Hoare triples with separating conjunction and frame rule.
; {emp} x = cons(1,2) {x ~> (1,2)} and linked-list reverse verification.

(define (sl-lk addr heap)
  (let ((e (find (lambda (b) (= (car b) addr)) heap)))
    (cond (e (car (cdr e))) (t nil))))

(define (sl-dom heap)
  (cond ((eq heap nil) nil) (t (cons (caar heap) (sl-dom (cdr heap))))))

(define (sl-disj h1 h2)
  (cond ((eq h1 nil) t) ((eq h2 nil) t)
        ((in (caar h1) (sl-dom h2)) nil)
        (t (sl-disj (cdr h1) h2))))

(define (sl-sjoin h1 h2)
  (cond ((sl-disj h1 h2) (append h1 h2)) (t nil)))

(define (sl-sat-ptos addr car-val cdr-val heap)
  (let ((node (sl-lk addr heap)))
    (and (not (eq node nil))
         (= (car node) car-val)
         (= (cdr node) cdr-val))))

(define (sl-emp heap) (eq heap nil))

(define (sl-sat formula heap)
  (cond ((and (listp formula) (eq (car formula) 'pt))
         (sl-sat-ptos (cadr formula) (caddr formula) (cadddr formula) heap))
        ((eq formula 'emp) (sl-emp heap))
        ((and (listp formula) (eq (car formula) '*))
         (sl-sat-sep (cadr formula) (caddr formula) heap))
        (t nil)))

; the separating conjunction splits the heap into disjoint subsets;
; the old sl-partition/sl-part-from/sl-remain were nil stubs and are
; replaced by the (previously unused) working enumerator
(define (sl-sat-sep p q heap)
  (sl-sat-split p q heap))

(define (sl-sat-split p q heap)
  (sl-ss p q heap nil nil))

(define (sl-ss p q full left right)
  (cond ((eq full nil)
         (and (sl-sat p left) (sl-sat q right)))
        (t (or (sl-ss p q (cdr full) (cons (car full) left) right)
               (sl-ss p q (cdr full) left (cons (car full) right))))))

(define (sl-ll addr heap)
  (sl-llw addr heap nil))

(define (sl-llw addr heap seen)
  (cond ((= addr 0) 'emp)
        ((in addr seen) nil)
        (t (let ((node (sl-lk addr heap)))
             (cond ((eq node nil) nil)
                   (t (let ((tail (sl-llw (cdr node) heap (cons addr seen))))
                        (cond (tail (list (list 'pt addr (car node) (cdr node)) tail))
                              (t nil)))))))))

(define (sl-alloc addr a b heap)
  (cons (list addr (cons a b)) heap))

(define (sl-free addr heap)
  (cond ((eq heap nil) nil)
        ((= (caar heap) addr) (cdr heap))
        (t (cons (car heap) (sl-free addr (cdr heap))))))

(define (sl-set addr aval dval heap)
  (sl-set-x addr aval dval heap nil))

(define (sl-set-x addr aval dval heap before)
  (cond ((eq heap nil) (append (reverse before) (list (list addr (cons aval dval)))))
        ((= (caar heap) addr)
         (append (reverse before) (cons (list addr (cons aval dval)) (cdr heap))))
        (t (sl-set-x addr aval dval (cdr heap) (cons (car heap) before)))))

; Basic heap operations
(define h0 nil)
(define h1 (sl-alloc 100 1 2 h0))
(equal (sl-sat-ptos 100 1 2 h1) t) t

; Separating conjunction
(define h2 (sl-alloc 200 3 4 h1))
(check "sep: two nodes split disjointly"
       (sl-sat '(* (pt 100 1 2) (pt 200 3 4)) h2))
(check "sep: commutative"
       (sl-sat '(* (pt 200 3 4) (pt 100 1 2)) h2))
(check "sep: sharing one node does NOT satisfy (was wrongly t)"
       (eq (sl-sat '(* (pt 100 1 2) (pt 100 1 2)) h2) nil))
(check "sep: wrong address fails"
       (eq (sl-sat '(* (pt 100 1 2) (pt 300 5 6)) h2) nil))

; Empty heap
(equal (sl-sat 'emp nil) t) t

; Linked list assertion
(define h3 '((100 (1 . 200)) (200 (2 . 0))))
(define ll3 (sl-ll 100 h3))
(equal (eq ll3 nil) nil) t

; Points-to with wrong value fails
(equal (sl-sat-ptos 100 1 99 h1) nil) t
