; ============================================================
; lib/math/combinatorics.lsp -- combinatorial generation
;
; Public:   (permutations l) -> list of all orderings
; Private:  comb-insert-everywhere (returns list of insertions of x into l)
; Depends:  none (uses prelude functions)
; Derived-from: pre-existing lib/math/combinatorics.lsp (verified unchanged
;               except helper prefixing)
; Assumes:  list elements comparable by equal
; ============================================================

(define (comb-insert-everywhere x l)
  "every list formed by inserting x into l at each position"
  (cond ((null l) (list (list x)))
        (t (cons (cons x l)
                 (map (lambda (r) (cons (car l) r))
                      (comb-insert-everywhere x (cdr l)))))))

(define (permutations l)
  "all orderings of the elements of l"
  (cond ((null l) (list nil))
        (t (foldr (lambda (p acc) (append (comb-insert-everywhere (car l) p) acc))
                  nil (permutations (cdr l))))))
