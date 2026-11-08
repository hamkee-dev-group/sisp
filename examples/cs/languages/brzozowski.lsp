; Brzozowski's regular expression derivatives.
; Regex: empty, epsilon, lit(c), union(R,S), concat(R,S), star(R).
; Derivative Da: Da(empty)=empty, Da(epsilon)=empty, Da(lit c)=epsilon if c=a, empty ow.
; Da(union R S)=union(Da(R),Da(S))
; Da(concat R S)=union(concat(Da(R),S), concat(nu(R),Da(S))) where nu(R)=epsilon if R nullable.
; Da(star R)=concat(Da(R), star(R))
; Matching: w in L(R) iff nu(Dw(R)) = epsilon.

(define (rx-nullable re)
  (cond ((eq re 'empty)  nil)
        ((eq re 'epsilon) t)
        ((and (consp re) (eq (car re) 'lit)) nil)
        ((eq (car re) 'union)
         (or (rx-nullable (cadr re)) (rx-nullable (caddr re))))
        ((eq (car re) 'concat)
         (and (rx-nullable (cadr re)) (rx-nullable (caddr re))))
        ((eq (car re) 'star) t)
        (t nil)))

(define (rx-nu re)
  (cond ((rx-nullable re) 'epsilon) (t 'empty)))

(define (rx-derivative re a)
  (cond ((eq re 'empty) 'empty)
        ((eq re 'epsilon) 'empty)
        ((and (consp re) (eq (car re) 'lit))
         (cond ((equal (cadr re) a) 'epsilon) (t 'empty)))
        ((eq (car re) 'union)
         (list 'union (rx-derivative (cadr re) a) (rx-derivative (caddr re) a)))
        ((eq (car re) 'concat)
         (let ((r (cadr re)) (s (caddr re)))
           (list 'union
             (list 'concat (rx-derivative r a) s)
             (list 'concat (rx-nu r) (rx-derivative s a)))))
        ((eq (car re) 'star)
         (list 'concat (rx-derivative (cadr re) a) re))
        (t 'empty)))

(define (rx-derivative-string re chars)
  (cond ((eq chars nil) re)
        (t (rx-derivative-string (rx-derivative re (car chars)) (cdr chars)))))

(define (rx-match re s)
  (rx-nullable (rx-derivative-string re (str-to-list s))))

; Build (a|b)*abb
(define a-lit (list 'lit "a"))
(define b-lit (list 'lit "b"))
(define a-or-b (list 'union a-lit b-lit))
(define star-a-or-b (list 'star a-or-b))
(define rx-abb (list 'concat (list 'concat (list 'concat star-a-or-b a-lit) b-lit) b-lit))

; Test matching
(print "=== Brzozowski Derivatives ===")
(print "Nullable of epsilon:") (print (rx-nullable 'epsilon))
(print "Nullable of empty:") (print (rx-nullable 'empty))
(print "Derivative Da(a):") (print (rx-derivative a-lit "a"))
(print "Derivative Da(b):") (print (rx-derivative a-lit "b"))
(print "Nullable Da(a):") (print (rx-nullable (rx-derivative a-lit "a")))
(print "Match (a|b)*abb on ababb:")  (print (rx-match rx-abb "ababb"))
(print "Match (a|b)*abb on aabb:")   (print (rx-match rx-abb "aabb"))
(print "Match (a|b)*abb on abb:")    (print (rx-match rx-abb "abb"))
(print "Match (a|b)*abb on ba:")     (print (rx-match rx-abb "ba"))
(print "Match (a|b)*abb on babb:")   (print (rx-match rx-abb "babb"))
(print "Match (a|b)*abb on empty:")  (print (rx-match rx-abb ""))
(print "=== Brzozowski Done ===")
