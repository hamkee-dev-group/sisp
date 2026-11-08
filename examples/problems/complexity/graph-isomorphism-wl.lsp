; graph-isomorphism-wl.lsp -- Weisfeiler-Lehman 1-D color refinement.
; Iteratively refine vertex colors: initial=degree, then sig=(color, sorted neighbor colors).
; Two graphs are WL-equivalent if their stable color histograms match.
; Test on small non-isomorphic graphs: C6, path-4, star, triangle.

(define (wl-init g)
  (let ((vs (map car g)))
    (map (lambda (v) (list v (length (cadr (assoc v g))))) vs)))

(define (wl-neighbors g v)
  (cadr (assoc v g)))

(define (wl-signature v coloring g)
  (let ((c (cadr (assoc v coloring)))
        (nbs (wl-neighbors g v)))
    (let ((nbcs (map (lambda (nb)
                       (cadr (assoc nb coloring)))
                     nbs)))
      (cons c (sort (lambda (a b) (< a b)) nbcs)))))

(define (wl-collect-sigs vs coloring g acc)
  (cond ((eq vs nil) acc)
        (t (wl-collect-sigs (cdr vs) coloring g
             (cons (list (car vs)
                         (wl-signature (car vs) coloring g))
                   acc)))))

(define (wl-unique-sigs siglist acc)
  (cond ((eq siglist nil) acc)
        ((wl-sig-in-list (car siglist) acc)
         (wl-unique-sigs (cdr siglist) acc))
        (t (wl-unique-sigs (cdr siglist)
             (cons (car siglist) acc)))))

(define (wl-sig-in-list sig lst)
  (cond ((eq lst nil) nil)
        ((wl-sigs-equal sig (car lst)) t)
        (t (wl-sig-in-list sig (cdr lst)))))

(define (wl-sigs-equal a b)
  (cond ((and (eq a nil) (eq b nil)) t)
        ((or (eq a nil) (eq b nil)) nil)
        ((= (car a) (car b))
         (wl-sigs-equal (cdr a) (cdr b)))
        (t nil)))

(define (wl-sig-less a b)
  (cond ((eq b nil) nil)
        ((eq a nil) t)
        ((< (car a) (car b)) t)
        ((> (car a) (car b)) nil)
        (t (wl-sig-less (cdr a) (cdr b)))))

(define (wl-sort-sigs sigs)
  (sort (lambda (a b) (wl-sig-less a b)) sigs))

(define (wl-find-color sig sorted idx)
  (cond ((eq sorted nil) idx)
        ((wl-sigs-equal sig (car sorted)) idx)
        (t (wl-find-color sig (cdr sorted) (+ idx 1)))))

(define (wl-recolor v coloring g sorted)
  (let ((sig (wl-signature v coloring g)))
    (list v (wl-find-color sig sorted 1))))

(define (wl-refine g coloring)
  (let ((vs (map car g)))
    (let ((sigs (wl-collect-sigs vs coloring g nil)))
      (let ((uniq (wl-unique-sigs (map cadr sigs) nil)))
        (let ((sorted (wl-sort-sigs uniq)))
          (map (lambda (v) (wl-recolor v coloring g sorted)) vs))))))

(define (wl-histo coloring)
  (let ((cs (map cadr coloring)))
    (sort (lambda (a b) (< a b)) cs)))

(define (wl-histo-eq h1 h2)
  (cond ((and (eq h1 nil) (eq h2 nil)) t)
        ((or (eq h1 nil) (eq h2 nil)) nil)
        ((= (car h1) (car h2))
         (wl-histo-eq (cdr h1) (cdr h2)))
        (t nil)))

(define (wl-stable-p old new)
  (wl-histo-eq (wl-histo old) (wl-histo new)))

(define (wl-iterate g coloring rem)
  (cond ((<= rem 0) coloring)
        (t (let ((nxt (wl-refine g coloring)))
             (cond ((wl-stable-p coloring nxt) nxt)
                   (t (wl-iterate g nxt (- rem 1))))))))

(define (wl-equivalent-p g1 g2 maxiters)
  (let ((c1 (wl-iterate g1 (wl-init g1) maxiters))
        (c2 (wl-iterate g2 (wl-init g2) maxiters)))
    (wl-histo-eq (wl-histo c1) (wl-histo c2))))

; --- test ---
(define wl-c6 '((1 (2 6)) (2 (1 3)) (3 (2 4)) (4 (3 5)) (5 (4 6)) (6 (5 1))))
(define wl-p4 '((1 (2)) (2 (1 3)) (3 (2 4)) (4 (3))))
(define wl-star '((1 (2 3 4)) (2 (1)) (3 (1)) (4 (1))))
(define wl-tri '((1 (2 3)) (2 (1 3)) (3 (1 2))))

(list
  (list (quote c6-self) (wl-equivalent-p wl-c6 wl-c6 10))
  (list (quote c6-vs-p4) (wl-equivalent-p wl-c6 wl-p4 10))
  (list (quote c6-vs-star) (wl-equivalent-p wl-c6 wl-star 10))
  (list (quote p4-vs-star) (wl-equivalent-p wl-p4 wl-star 10))
  (list (quote tri-self) (wl-equivalent-p wl-tri wl-tri 10))
  (list (quote tri-vs-c6) (wl-equivalent-p wl-tri wl-c6 10)))
