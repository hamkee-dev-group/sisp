; Algorithm W: Hindley-Milner type inference with unification and let-polymorphism.

(define (hm-tv ty) (and (listp ty) (eq (car ty) 'tvar)))
(define (hm-tc ty) (and (listp ty) (eq (car ty) 'tcon)))
(define (hm-tf ty) (and (listp ty) (eq (car ty) '->)))
(define (hm-ta ty) (and (listp ty) (eq (car ty) 'forall)))
(define (hm-vn ty) (cadr ty))

(define hm-ct 0)
(define (hm-rf) (setq hm-ct 0))
(define (hm-fr) (setq hm-ct (+ hm-ct 1)) (list 'tvar hm-ct))

(define (hm-ft ty)
  (cond ((hm-tv ty) (list ty))
        ((hm-tc ty) nil)
        ((hm-tf ty) (append (hm-ft (cadr ty)) (hm-ft (caddr ty))))
        ((hm-ta ty) (remove (lambda (v) (equal v (cadr ty))) (hm-ft (caddr ty))))
        (t nil)))

(define (hm-eq a b)
  (cond ((hm-tv a) (cond ((hm-tv b) (= (hm-vn a) (hm-vn b))) (t nil)))
        ((hm-tc a) (cond ((hm-tc b) (eq (cadr a) (cadr b))) (t nil)))
        ((hm-tf a) (cond ((hm-tf b) (cond ((hm-eq (cadr a) (cadr b)) (hm-eq (caddr a) (caddr b))) (t nil))) (t nil)))
        (t (equal a b))))

(define (hm-occ var ty) (any (lambda (v) (equal v var)) (hm-ft ty)))

(define (hm-sl var s)
  (let ((e (find (lambda (b) (hm-eq (car b) var)) s)))
    (cond (e (cadr e)) (t var))))

(define (hm-as ty s)
  (cond ((hm-tv ty) (hm-sl ty s)) ((hm-tc ty) ty)
        ((hm-tf ty) (list '-> (hm-as (cadr ty) s) (hm-as (caddr ty) s)))
        ((hm-ta ty) (list 'forall (cadr ty) (hm-as (caddr ty) s)))
        (t ty)))

(define (hm-cs s2 s1) (append (map (lambda (b) (list (car b) (hm-as (cadr b) s2))) s1) s2))

(define (hm-un a b) (hm-ux a b nil))

(define (hm-ux a b s)
  (let ((x (hm-as a s)) (y (hm-as b s)))
    (cond ((hm-eq x y) s)
          ((hm-tv x) (cond ((hm-occ x y) nil) (t (cons (list x y) s))))
          ((hm-tv y) (cond ((hm-occ y x) nil) (t (cons (list y x) s))))
          ((hm-tf x) (cond ((hm-tf y)
                            (let ((s1 (hm-ux (cadr x) (cadr y) s)))
                              (cond (s1 (hm-ux (caddr x) (caddr y) s1)) (t nil))))
                           (t nil)))
          (t nil))))

(define (hm-gen env ty)
  (let ((ftv (hm-ft ty))
        (etv (foldl (lambda (acc b) (append acc (hm-ft (cdr b)))) nil env)))
    (foldl (lambda (sch v) (cond ((in v etv) sch) (t (list 'forall v sch)))) ty (hm-dedup ftv))))

(define (hm-dedup xs)
  (cond ((null xs) nil)
        ((in (car xs) (cdr xs)) (hm-dedup (cdr xs)))
        (t (cons (car xs) (hm-dedup (cdr xs))))))

(define (hm-inst sch)
  (cond ((hm-ta sch) (let ((nv (hm-fr))) (hm-inst (hm-as (caddr sch) (list (list (cadr sch) nv))))))
        (t sch)))

(define (hm-lk nm env)
  (let ((e (find (lambda (b) (eq (car b) nm)) env))) (cond (e (cdr e)) (t nil))))

(define (hm-ex nm sch env) (cons (cons nm sch) env))

(define (hm-infer expr env)
  (cond ((numberp expr) (list (list 'tcon 'int) nil))
        ((symbolp expr)
         (let ((sch (hm-lk expr env)))
           (cond (sch (list (hm-inst sch) nil)) (t (list nil nil)))))
        ((eq (car expr) 'lambda)
         (let ((arg (caadr expr)) (body (caddr expr)))
           (let ((at (hm-fr)))
             (let ((eenv (hm-ex arg (list 'forall (hm-fr) at) env)))
               (let ((r (hm-infer body eenv)))
                 (cond ((car r) (list (list '-> at (car r)) (cadr r))) (t nil)))))))
        ((eq (car expr) 'let)
         (let ((var (car (car (cadr expr))))
               (val (cadr (car (cadr expr))))
               (body (caddr expr)))
           (let ((vr (hm-infer val env)))
             (cond ((car vr) (let ((sch (hm-gen env (car vr)))) (hm-infer body (hm-ex var sch env)))) (t nil)))))
        (t (let ((fr (hm-infer (car expr) env)))
             (cond ((car fr) (cond ((hm-tf (car fr))
                                    (let ((ar (hm-infer (cadr expr) env)))
                                      (cond ((car ar)
                                             (let ((s (hm-un (cadr (car fr)) (car ar))))
                                               (cond (s (list (hm-as (caddr (car fr)) s) s)) (t nil))))
                                            (t nil))))
                                   (t nil)))
                   (t nil))))))

(define (hm-it expr) (hm-rf) (let ((r (hm-infer expr nil))) (cond ((car r) (car r)) (t 'type-error))))

; Tests
; identity: (lambda (x) x) has type (-> a a)
(define r1 (hm-it '(lambda (x) x)))
(check "identity type is (-> a a)"
       (and (eq (car r1) '->) (equal (cadr r1) (caddr r1))))
; constant: (lambda (x) (lambda (y) x))
(define r2 (hm-it '(lambda (x) (lambda (y) x))))
(check "const type is an arrow"
       (eq (car r2) '->))
; application: ((lambda (x) x) 42) has type int
(define r3 (hm-it '((lambda (x) x) 42)))
(check "identity applied to int is int"
       (equal r3 (list 'tcon 'int)))
; let polymorophism (was type-error: the binding was parsed whole)
(define r4 (hm-it '(let ((id (lambda (x) x))) (id 5))))
(check "let-polymorphism is int"
       (equal r4 (list 'tcon 'int)))
