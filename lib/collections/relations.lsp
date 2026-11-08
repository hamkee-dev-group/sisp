; Finite set and relation algebra. Relations are sets of two-element lists.
; Public: enumerate, pairs-such-that, k-subsets, rel-domain, rel-range,
;         rel-inverse, rel-image, rel-compose, function-p, injective-p,
;         lfp, transitive-closure
; Depends: none (uses finite-set primitives and the embedded prelude)

(define (enumerate class carrier)
  "The members of a class that appear in a finite carrier set.
  (enumerate (complement {1}) {1 2 3}) => {2 3}
Note: This is how a comprehension set becomes finite.
See: complement filter set-to-list"
  (filter (lambda (value) (in value class)) carrier))

(define (pairs-such-that predicate left right)
  "Pairs from two sets satisfying a two-argument predicate.
  (pairs-such-that < {1 2} {2}) => {(1 2)}
See: cartesian-product filter"
  (filter (lambda (pair) (predicate (car pair) (cadr pair)))
          (cartesian-product left right)))

(define (k-subsets set size)
  "All subsets of a given size.
  (k-subsets {1 2 3} 2) => {{1 2} {1 3} {2 3}}
See: powerset"
  (filter (lambda (subset) (= (length subset) size)) (powerset set)))

(define (rel-domain relation)
  "The first components of a relation's pairs.
  (rel-domain {(1 2)}) => {1}
See: rel-range rel-inverse"
  (map (lambda (pair) (car pair)) relation))

(define (rel-range relation)
  "The second components of a relation's pairs.
  (rel-range {(1 2)}) => {2}
See: rel-domain"
  (map (lambda (pair) (cadr pair)) relation))

(define (rel-inverse relation)
  "The relation with every pair reversed.
  (rel-inverse {(1 2)}) => {(2 1)}
See: rel-domain rel-range"
  (map (lambda (pair) (list (cadr pair) (car pair))) relation))

(define (rel-image relation inputs)
  "What a relation maps a set of inputs to.
  (rel-image {(1 2)} {1}) => {2}
See: rel-range rel-compose"
  (rel-range (filter (lambda (pair) (in (car pair) inputs)) relation)))

(define (rel-compose left right)
  "Relational composition of left with right.
  (rel-compose {(1 2)} {(2 3)}) => {(1 3)}
See: rel-image transitive-closure"
  (list-to-set
    (foldl
      (lambda (result left-pair)
        (append result
          (map (lambda (right-pair)
                 (list (car left-pair) (cadr right-pair)))
               (filter
                 (lambda (right-pair)
                   (equal (cadr left-pair) (car right-pair)))
                 (set-to-list right)))))
      nil
      (set-to-list left))))

(define (function-p relation)
  "Whether a relation maps each input to at most one output.
  (function-p {(1 2)}) => t
  (function-p {(1 2) (1 3)}) => nil
See: injective-p rel-domain"
  (every
    (lambda (left-pair)
      (every
        (lambda (right-pair)
          (or (not (equal (car left-pair) (car right-pair)))
              (equal (cadr left-pair) (cadr right-pair))))
        relation))
    relation))

(define (injective-p relation)
  "Whether a relation maps distinct inputs to distinct outputs.
  (injective-p {(1 2)}) => t
See: function-p"
  (every
    (lambda (left-pair)
      (every
        (lambda (right-pair)
          (or (not (equal (cadr left-pair) (cadr right-pair)))
              (equal (car left-pair) (car right-pair))))
        relation))
    relation))

(define (lfp function initial)
  "Least fixed point: apply a function until it stops changing.
  (lfp (lambda (set) set) {1}) => {1}
Note: Diverges if the sequence never settles.
See: transitive-closure"
  (let ((next (function initial)))
    (cond ((equal next initial) initial)
          (t (lfp function next)))))

(define (transitive-closure relation)
  "The smallest transitive relation containing this one.
  (transitive-closure {(1 2) (2 3)}) => {(1 2) (2 3) (1 3)}
See: rel-compose lfp"
  (lfp (lambda (current)
         (union current (rel-compose current relation)))
       relation))
