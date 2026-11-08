; ============================================================
; name: kmeans
; file: algorithms/ai/kmeans.lsp
; subject: ai
; capability: cluster unsupervised
; aliases: "k-means" "lloyd" "clustering" "vector quantization"
; signature: (kmeans points centroids iterations) -> centroids
; complexity: O(n k d) per iteration (Lloyd)
; see-also: knn-classifier
; derived-from: Lloyd 1982
; status: example (tested)
; checks: 2 - convergence to known clusters
; ============================================================
; Lloyd's k-means alternates nearest-centroid assignment and centroid recomputation.
(define (km-distance2 left right)
  (cond ((eq left nil) 0)
        (t (+ (^ (- (car left) (car right)) 2) (km-distance2 (cdr left) (cdr right))))))
(define (km-nearest point centroids index best best-distance)
  (cond ((eq centroids nil) best)
        (t (let ((distance (km-distance2 point (car centroids))))
          (cond ((or (eq best nil) (< distance best-distance))
                 (km-nearest point (cdr centroids) (+ index 1) index distance))
                (t (km-nearest point (cdr centroids) (+ index 1) best best-distance)))))))
(define (km-dimensions count index)
  (cond ((= index count) nil) (t (cons index (km-dimensions count (+ index 1))))))
(define (km-centroid points fallback)
  (cond ((eq points nil) fallback)
        (t (map (lambda (index) (/ (foldl (lambda (sum point) (+ sum (nth index point)))
                              0 points) (length points)))
             (km-dimensions (length (car points)) 0)))))
(define (km-recenter points centroids)
  (map (lambda (index) (km-centroid
    (filter (lambda (point) (= (km-nearest point centroids 0 nil 0) index)) points)
    (nth index centroids))) (km-dimensions (length centroids) 0)))
(define (kmeans points centroids iterations)
  (cond ((= iterations 0) centroids)
        (t (kmeans points (km-recenter points centroids) (- iterations 1)))))
(kmeans '((0 0) (0 2) (10 10) (10 12)) '((0 0) (10 10)) 3) ; => ((0 1) (10 11))
(map (lambda (point) (km-nearest point '((0 1) (10 11)) 0 nil 0)) '((0 0) (10 12))) ; => (0 1)


; ---- checks ----
(check "kmeans: two clusters converge to their centroids"
       (equal (kmeans '((0 0) (0 2) (10 10) (10 12)) '((0 0) (10 10)) 3)
              '((0 1) (10 11))))

(check "kmeans: points assigned to the nearest cluster"
       (equal (map (lambda (point) (km-nearest point '((0 1) (10 11)) 0 nil 0))
                   '((0 0) (10 12)))
              '(0 1)))
