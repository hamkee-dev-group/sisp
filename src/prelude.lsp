; prelude.lsp -- the SISP standard library, loaded at boot.
; This file is the source of truth: the Makefile embeds it into
; libsisp.h at build time, so the binary stays self-contained.
; It is plain SISP and can be run directly: src/sisp src/prelude.lsp

; The universal set: everything is equal to itself.
; ---- cxr accessors: every two-to-four-step car/cdr path ----
(define (caar x) "Equivalent to (car (car x)). See: car cdr" (car (car x)))
(define (cadr x) "Equivalent to (car (cdr x)). See: car cdr" (car (cdr x)))
(define (cdar x) "Equivalent to (cdr (car x)). See: car cdr" (cdr (car x)))
(define (cddr x) "Equivalent to (cdr (cdr x)). See: car cdr" (cdr (cdr x)))
(define (caaar x) "Equivalent to (car (car (car x))). See: car cdr" (car (car (car x))))
(define (caadr x) "Equivalent to (car (car (cdr x))). See: car cdr" (car (car (cdr x))))
(define (cadar x) "Equivalent to (car (cdr (car x))). See: car cdr" (car (cdr (car x))))
(define (caddr x) "Equivalent to (car (cdr (cdr x))). See: car cdr" (car (cdr (cdr x))))
(define (cdaar x) "Equivalent to (cdr (car (car x))). See: car cdr" (cdr (car (car x))))
(define (cdadr x) "Equivalent to (cdr (car (cdr x))). See: car cdr" (cdr (car (cdr x))))
(define (cddar x) "Equivalent to (cdr (cdr (car x))). See: car cdr" (cdr (cdr (car x))))
(define (cdddr x) "Equivalent to (cdr (cdr (cdr x))). See: car cdr" (cdr (cdr (cdr x))))
(define (caaaar x) "Equivalent to (car (car (car (car x)))). See: car cdr" (car (caaar x)))
(define (caaadr x) "Equivalent to (car (car (car (cdr x)))). See: car cdr" (car (caadr x)))
(define (caadar x) "Equivalent to (car (car (cdr (car x)))). See: car cdr" (car (cadar x)))
(define (caaddr x) "Equivalent to (car (car (cdr (cdr x)))). See: car cdr" (car (caddr x)))
(define (cadaar x) "Equivalent to (car (cdr (car (car x)))). See: car cdr" (car (cdaar x)))
(define (cadadr x) "Equivalent to (car (cdr (car (cdr x)))). See: car cdr" (car (cdadr x)))
(define (caddar x) "Equivalent to (car (cdr (cdr (car x)))). See: car cdr" (car (cddar x)))
(define (cadddr x) "Equivalent to (car (cdr (cdr (cdr x)))). See: car cdr" (car (cdddr x)))
(define (cdaaar x) "Equivalent to (cdr (car (car (car x)))). See: car cdr" (cdr (caaar x)))
(define (cdaadr x) "Equivalent to (cdr (car (car (cdr x)))). See: car cdr" (cdr (caadr x)))
(define (cdadar x) "Equivalent to (cdr (car (cdr (car x)))). See: car cdr" (cdr (cadar x)))
(define (cdaddr x) "Equivalent to (cdr (car (cdr (cdr x)))). See: car cdr" (cdr (caddr x)))
(define (cddaar x) "Equivalent to (cdr (cdr (car (car x)))). See: car cdr" (cdr (cdaar x)))
(define (cddadr x) "Equivalent to (cdr (cdr (car (cdr x)))). See: car cdr" (cdr (cdadr x)))
(define (cdddar x) "Equivalent to (cdr (cdr (cdr (car x)))). See: car cdr" (cdr (cddar x)))
(define (cddddr x) "Equivalent to (cdr (cdr (cdr (cdr x)))). See: car cdr" (cdr (cdddr x)))
; ---- fundamental list operations ----
(define (filter f l)
  "The elements satisfying a predicate, preserving list or finite-set type.
  (filter evenp (quote (1 2 3 4))) => (2 4)
  (filter evenp {1 2 3 4}) => {2 4}
See: remove find"
  (cond ((setp l) (list-to-set (filter f (set-to-list l))))
        ((eq l nil) nil)
        ((f (car l)) (cons (car l) (filter f (cdr l))))
        (t (filter f (cdr l)))))
(define (remove f l)
  "The elements NOT satisfying a predicate, preserving collection type.
  (remove evenp (quote (1 2 3 4))) => (1 3)
See: filter"
  (cond ((setp l) (list-to-set (remove f (set-to-list l))))
        ((eq l nil) nil)
        ((f (car l)) (remove f (cdr l)))
        (t (cons (car l) (remove f (cdr l))))))
(define (find f l)
  "The first element satisfying a predicate, or nil. Set order is unspecified.
  (find evenp (quote (1 2 3))) => 2
  (find evenp (quote (1 3))) => nil
See: filter any contains"
  (cond ((setp l) (find f (set-to-list l)))
        ((eq l nil) nil)
        ((f (car l)) (car l))
        (t (find f (cdr l)))))
(define (foldr f init l)
  "Fold from the right. Finite sets are traversed in unspecified order.
  (foldr cons nil (quote (1 2))) => (1 2)
Bugs: Not tail recursive: it runs out of C stack somewhere around 16000 elements. Prefer foldl where the operation allows it.
See: foldl"
  (cond ((setp l) (foldr f init (set-to-list l)))
        ((eq l nil) init)
        (t (f (car l) (foldr f init (cdr l))))))
(define (foldl f acc l)
  "Fold from the left, carrying an accumulator. Set order is unspecified.
  (foldl + 0 (quote (1 2 3))) => 6
Note: Tail recursive, so it handles long lists. foldr is not.
See: foldr map"
  (cond ((setp l) (foldl f acc (set-to-list l)))
        ((eq l nil) acc)
        (t (foldl f (f acc (car l)) (cdr l)))))
(define (every f l)
  "Whether every element of a list or finite set satisfies a predicate.
  (every evenp (quote (2 4))) => t
See: any"
  (cond ((setp l) (every f (set-to-list l)))
        ((eq l nil) t)
        ((f (car l)) (every f (cdr l)))
        (t nil)))
(define (any f l)
  "Whether some element of a list or finite set satisfies a predicate.
  (any evenp (quote (1 2))) => t
  (any evenp (quote (1 3))) => nil
See: every find"
  (cond ((setp l) (any f (set-to-list l)))
        ((eq l nil) nil)
        ((f (car l)) t)
        (t (any f (cdr l)))))
(define (foreach f l)
  "Apply a function to every element for its effect only. Set order is unspecified.
  (progn (foreach print nil) (quote done)) => done
See: map"
  (cond ((setp l) (foreach f (set-to-list l)))
        ((eq l nil) nil)
        (t (f (car l)) (foreach f (cdr l)))))
(define (count f l)
  "How many elements of a list or finite set satisfy a predicate.
  (count evenp (quote (1 2 3 4))) => 2
See: length"
  (cond ((setp l) (count f (set-to-list l)))
        ((eq l nil) 0)
        ((f (car l)) (+ 1 (count f (cdr l))))
        (t (count f (cdr l)))))
(define (flatmap f l)
  "Map, then concatenate the resulting lists -- one level only.
  (flatmap (lambda (x) (list x x)) (quote (1 2))) => (1 1 2 2)
See: deep-flatmap map append"
  (foldr append nil (map f l)))
(define (check name ok)
  "Print a pass/fail line for a named condition. The corpus uses it as its test harness.
  (progn (check x t) (quote done)) => done
See: print"
  (cond (ok (print (list (quote pass) name))) (t (print (list (quote FAIL) name)))))
