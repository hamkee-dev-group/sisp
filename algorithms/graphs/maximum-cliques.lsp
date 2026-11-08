; ============================================================
; name: maximum-cliques
; file: algorithms/graphs/maximum-cliques.lsp
; subject: graph
; capability: search enumerate
; aliases: "bron-kerbosch" "maximal cliques" "maximum clique"
; signature: (find-maximal-cliques) -> list-of-maximal-cliques
;            (bron-kerbosch r p x) records cliques in *all-maximal*
; complexity: O(3^(n/3)) with Tomita's pivot rule; this file pivots on
;             an arbitrary vertex of P u X
; see-also: bipartite-matching
; derived-from: Bron & Kerbosch 1973; Tomita, Tanaka & Takahashi 2006
; status: example (tested)
; checks: 10 - all maximal cliques enumerated with pivoting
; ============================================================
(print "=== Maximum Cliques ===")

; ---------- graph representation ----------
; Undirected graph: set of edges (each edge is a list of 2 vertices)

(define graph-edges
  (list-to-set (list (list 1 2) (list 2 3) (list 3 1) (list 3 4))))

(define vertices (list-to-set (list 1 2 3 4)))

(define (edge-exists u v)
  (or (in (list u v) graph-edges)
      (in (list v u) graph-edges)))

(define (neighbors-of v)
  (foldl (lambda (acc e)
    (cond ((= (car e) v) (cons (cadr e) acc))
          ((= (cadr e) v) (cons (car e) acc))
          (t acc)))
    nil
    (set-to-list graph-edges)))

(define (neighbors-set v)
  (list-to-set (neighbors-of v)))

; ---------- Bron-Kerbosch with pivoting ----------
; Algorithm: BK(R, P, X)
; R = current clique (list), P = candidate set, X = excluded set
; Returns list of all maximal cliques (each as sorted list)

(setq *all-maximal* nil)

(define (bron-kerbosch r p x)
  (cond ((and (equal p {}) (equal x {}))
    (setq *all-maximal* (cons (sort-clique r) *all-maximal*))
    nil)
        (t
          (let ((u (choose-pivot p x)))
            (foldl (lambda (ignored v)
              (cond ((not (in v (neighbors-set u)))
                (let ((nv (neighbors-set v)))
                  (bron-kerbosch (cons v r)
                    (cap p nv)
                    (cap x nv))
                  (setq p (diff p (list-to-set (list v))))
                  (setq x (union x (list-to-set (list v))))
                  nil))
              (t nil)))
              nil
              (set-to-list p))
            nil))))

(define (choose-pivot p x)
  (let ((pu (union p x)))
    (cond ((equal pu {}) nil)
          (t (find (lambda (v) t) pu)))))

(define (sort-clique c)
  (cond ((eq c nil) nil)
        (t (let ((m (clique-min c)))
          (cons m (sort-clique (clique-remove m c)))))))

(define (clique-min c)
  (cond ((eq (cdr c) nil) (car c))
        ((< (car c) (clique-min (cdr c))) (car c))
        (t (clique-min (cdr c)))))

(define (clique-remove x c)
  (cond ((eq c nil) nil)
        ((= (car c) x) (cdr c))
        (t (cons (car c) (clique-remove x (cdr c))))))

; ---------- find all maximal cliques ----------
(define (find-maximal-cliques)
  (setq *all-maximal* nil)
  (bron-kerbosch nil vertices {})
  *all-maximal*)

(define maximal-cliques (find-maximal-cliques))
(print "Maximal cliques (Bron-Kerbosch):")
(print maximal-cliques)

; Check that {1,2,3} and {3,4} are found
(define (clique-present clique)
  (let ((sorted-c (sort-clique clique)))
    (foldl (lambda (acc c)
      (or acc (equal sorted-c c)))
      nil maximal-cliques)))

(check "clique-1-2-3-maximal" (clique-present (list 1 2 3)))
(check "clique-3-4-maximal" (clique-present (list 3 4)))
(check "exactly-2-maximal-cliques" (= (length maximal-cliques) 2))

; ---------- maximum clique ----------
(define (maximum-clique)
  (foldl (lambda (best c)
    (cond ((> (length c) (length best)) c)
          (t best)))
    nil maximal-cliques))

(define max-clique (maximum-clique))
(print "Maximum clique:")
(print max-clique)
(check "maximum-clique-is-1-2-3" (equal (sort-clique max-clique) (list 1 2 3)))
(check "maximum-size-is-3" (= (length max-clique) 3))

; ---------- exhaustive subset verification ----------
; Verify: every maximal clique from BK is indeed a clique and maximal.

(define (is-clique vertices-set)
  (let ((vl (set-to-list vertices-set)))
    (cond ((<= (length vl) 1) t)
          (t (foldl (lambda (acc v)
            (cond ((not acc) nil)
                  (t (foldl (lambda (inner u)
                    (cond ((not inner) nil)
                          ((= u v) inner)
                          (t (edge-exists v u))))
                    t vl))))
              t vl)))))

(define (is-maximal-clique c all-verts)
  (cond ((not (is-clique (list-to-set c))) nil)
        (t (let ((supersets
              (filter (lambda (s)
                (and (is-clique s)
                     (in-set s c)
                     (> (length s) (length c))))
                (all-supersets c all-verts))))
          (= (length supersets) 0)))))

(define (in-set big small)
  (foldl (lambda (acc x) (and acc (in x big))) t small))

(define (all-supersets clique all-verts)
  (let ((remaining (set-to-list (diff (list-to-set all-verts) (list-to-set clique)))))
    (foldl (lambda (acc vert)
      (let ((extended (append clique (list vert))))
        (cons (list-to-set extended) acc)))
      nil remaining)))

(define all-vert-list (set-to-list vertices))

; Check each found clique is maximal
(check "bk-clique1-is-clique" (is-clique (list-to-set (list 1 2 3))))
(check "bk-clique2-is-clique" (is-clique (list-to-set (list 3 4))))

(define (check-maximality)
  (foldl (lambda (acc c)
    (and acc
      (let ((supers (filter (lambda (v)
            (not (in v (list-to-set c))))
            all-vert-list)))
        (foldl (lambda (inner v)
          (cond ((not inner) nil)
                (t (let ((ext (append c (list v))))
                  (not (is-clique (list-to-set ext)))))))
          t supers))))
    t maximal-cliques))

(check "maximal-verified-by-superset" (check-maximality))

; Exhaustive: find all cliques and verify that BK found exactly the maximal ones
(define (all-subsets s)
  (set-to-list (powerset s)))

(define (find-all-cliques)
  (filter (lambda (sub)
    (is-clique sub))
    (map (lambda (s) (set-to-list s))
         (set-to-list (powerset vertices)))))

(define all-cliques (find-all-cliques))
(print "Total cliques found exhaustively:")
(print (length all-cliques))

; Verify no maximal clique is missed: every maximal clique in exhaustive
; should match one from BK
(define (exhaustive-maximal-cliques)
  (filter (lambda (c)
    (is-maximal-clique c all-vert-list))
    all-cliques))

(define exh-max (exhaustive-maximal-cliques))
(print "Exhaustive maximal cliques:")
(print exh-max)

(check "bk-matches-exhaustive"
  (and (= (length maximal-cliques) (length exh-max))
       (foldl (lambda (acc c)
         (let ((sc (sort-clique c)))
           (and acc
             (foldl (lambda (found bk-c)
               (or found (equal sc bk-c)))
               nil maximal-cliques))))
         t exh-max)))

; Maximum size check
(check "max-size-matches-exhaustive"
  (= (length max-clique)
     (foldl (lambda (best c)
       (cond ((> (length c) best) (length c))
             (t best)))
       0 all-cliques)))

(print "=== Maximum Cliques Done ===")
