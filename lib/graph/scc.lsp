; ============================================================
; lib/graph/scc.lsp -- strongly connected components (Kosaraju)
;
; Public:   (strongly-connected-components vertices edges) -> list of
;           vertex lists, one per SCC, in a topological order of the
;           condensation (sources first)
; Private:  scc-member-p, scc-add-edge, scc-build-adj, scc-neighbors,
;           scc-dfs1, scc-dfs1-edges, scc-dfs2, scc-dfs2-edges,
;           scc-pass2, scc-reverse-edges
; Depends:  lib/collections/alist.lsp
; Derived-from: CLRS 22.5 (Kosaraju's two-pass algorithm)
; Assumes:  edges = ((u v) ...) directed; vertices comparable with equal
;
; Contract: returns the partition of all vertices into maximal strongly
;           connected components.  Isolated vertices are singletons.
; ============================================================
(load "lib/collections/alist.lsp")

(define (scc-member-p x l)
  (cond ((null l) nil)
        ((equal x (car l)) t)
        (t (scc-member-p x (cdr l)))))

; adjacency alist vertex -> neighbor list, preserving edge order
(define (scc-add-edge adj u v)
  (let ((entry (alist-get adj u)))
    (cond ((eq entry nil) (cons (cons u (list v)) adj))
          (t (cons (cons u (append entry (list v)))
                   (remove (lambda (p) (equal (car p) u)) adj))))))

(define (scc-build-adj edges adj)
  (cond ((null edges) (reverse adj))
        (t (scc-build-adj (cdr edges)
                          (scc-add-edge adj (caar edges) (cadar edges))))))

(define (scc-neighbors adj v)
  (let ((entry (alist-get adj v)))
    (cond ((eq entry nil) nil) (t entry))))

(define (scc-reverse-edges edges acc)
  (cond ((null edges) (reverse acc))
        (t (scc-reverse-edges (cdr edges)
                              (cons (list (cadar edges) (caar edges)) acc)))))

; ---- pass 1: finish order on G ----
; state = (finish-stack . visited)

(define (scc-dfs1 adj v state)
  (scc-dfs1-edges adj (scc-neighbors adj v)
                  (cons (car state) (cons v (cdr state))) v))

(define (scc-dfs1-edges adj ns state v)
  (cond ((null ns) (cons (cons v (car state)) (cdr state)))
        ((scc-member-p (car ns) (cdr state))
         (scc-dfs1-edges adj (cdr ns) state v))
        (t (scc-dfs1-edges adj (cdr ns)
                           (scc-dfs1 adj (car ns) state) v))))

; ---- pass 2: collect components on G^T in decreasing finish order ----
; state = (current-component . visited)

(define (scc-dfs2 radj v state)
  (scc-dfs2-edges radj (scc-neighbors radj v)
                  (cons (cons v (car state)) (cons v (cdr state)))))

(define (scc-dfs2-edges radj ns state)
  (cond ((null ns) state)
        ((scc-member-p (car ns) (cdr state))
         (scc-dfs2-edges radj (cdr ns) state))
        (t (scc-dfs2-edges radj (cdr ns)
                           (scc-dfs2 radj (car ns) state)))))

(define (scc-pass2 radj finish visited comps)
  (cond ((null finish) (reverse comps))
        ((scc-member-p (car finish) visited)
         (scc-pass2 radj (cdr finish) visited comps))
        (t (let ((r (scc-dfs2 radj (car finish) (cons nil visited))))
             (scc-pass2 radj (cdr finish) (cdr r)
                        (cons (reverse (car r)) comps))))))

(define (strongly-connected-components vertices edges)
  "partition of vertices into maximal strongly connected components"
  (let* ((adj (scc-build-adj edges nil))
         (radj (scc-build-adj (scc-reverse-edges edges nil) nil))
         (finish (car (scc-finish-all adj vertices (cons nil nil)))))
    (scc-pass2 radj finish nil nil)))

(define (scc-finish-all adj vertices state)
  (cond ((null vertices) state)
        ((scc-member-p (car vertices) (cdr state))
         (scc-finish-all adj (cdr vertices) state))
        (t (scc-finish-all adj (cdr vertices)
                           (scc-dfs1 adj (car vertices) state)))))
