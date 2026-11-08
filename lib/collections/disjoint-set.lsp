; ============================================================
; lib/collections/disjoint-set.lsp -- disjoint-set forest (union-find)
;
; A DSU is a dotted pair (parent-alist . rank-alist).
; Each operation returns a new DSU; path compression is applied.
;
; Public:   (dsu-init elements) -> dsu
;           (dsu-find dsu x) -> (root . new-dsu)
;           (dsu-union dsu x y) -> (changed . new-dsu)
;           (dsu-connected-p dsu x y) -> t | nil
; Private:  dsu-parent-get, dsu-rank-get, dsu-parent-set, dsu-rank-set
; Depends:  lib/collections/alist.lsp
; Derived-from: examples/math/data-structures/disjoint-set-forests.lsp
; Assumes:  elements are comparable with = (integer equality)
; ============================================================
(load "lib/collections/alist.lsp")

(define (dsu-init elements)
  (let ((parents (foldl (lambda (acc e) (alist-set acc e e)) nil elements))
        (ranks (foldl (lambda (acc e) (alist-set acc e 0)) nil elements)))
    (cons parents ranks)))

(define (dsu-parent-get dsu x) (alist-get (car dsu) x))
(define (dsu-rank-get dsu x) (alist-get (cdr dsu) x))
(define (dsu-parent-set dsu x p) (cons (alist-set (car dsu) x p) (cdr dsu)))
(define (dsu-rank-set dsu x r) (cons (car dsu) (alist-set (cdr dsu) x r)))

(define (dsu-find dsu x)
  (let ((p (dsu-parent-get dsu x)))
    (cond ((eq p nil) (cons nil dsu))
          ((= p x) (cons x dsu))
          (t (let* ((rec (dsu-find dsu p))
                    (root (car rec))
                    (dsu2 (cdr rec)))
               (cons root (dsu-parent-set dsu2 x root)))))))

(define (dsu-union dsu x y)
  (let* ((fx (dsu-find dsu x)) (rx (car fx)) (dsu1 (cdr fx))
         (fy (dsu-find dsu1 y)) (ry (car fy)) (dsu2 (cdr fy)))
    (cond ((eq rx nil) (cons nil dsu))
          ((eq ry nil) (cons nil dsu))
          ((= rx ry) (cons nil dsu2))
          (t (let ((rankx (dsu-rank-get dsu2 rx))
                   (ranky (dsu-rank-get dsu2 ry)))
               (cond ((< rankx ranky) (cons t (dsu-parent-set dsu2 rx ry)))
                     ((> rankx ranky) (cons t (dsu-parent-set dsu2 ry rx)))
                     (t (cons t (dsu-rank-set (dsu-parent-set dsu2 ry rx)
                                              rx (+ rankx 1))))))))))

(define (dsu-connected-p dsu x y)
  (let* ((fx (dsu-find dsu x)) (rx (car fx)) (dsu1 (cdr fx))
         (fy (dsu-find dsu1 y)) (ry (car fy)))
    (and (not (eq rx nil)) (not (eq ry nil)) (= rx ry))))
