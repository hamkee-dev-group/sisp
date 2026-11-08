; ============================================================
; name: convex-hull
; file: algorithms/geometry/convex-hull.lsp
; subject: geometry
; capability: hull geometry
; aliases: "convex hull" "graham scan" "jarvis march" "gift wrapping"
; signature: (graham-scan points) -> hull ; (jarvis-march points) -> hull
; complexity: Graham O(n log n); Jarvis O(nh)
; see-also: closest-pair
; derived-from: Graham 1972; Jarvis 1973; CLRS 33.3
; status: example (tested)
; checks: 10 - Graham and Jarvis cross-checked to agree on the hull
; ============================================================
; Convex Hull — Graham Scan
; Points: ((0,3) (2,2) (1,1) (2,1) (3,0) (0,0) (3,3))
; Expected hull: ((0,0) (3,0) (3,3) (0,3))
; Cross-verify with Jarvis March (gift wrapping).
; ============================================================

(print "=== Convex Hull (Graham Scan) ===")


(define (px p) (car p))
(define (py p) (cadr p))

; Cross product: (p1-p0) x (p2-p0)
; >0 means counter-clockwise, <0 means clockwise, =0 means collinear
(define (cross p0 p1 p2)
  (- (* (- (px p1) (px p0)) (- (py p2) (py p0)))
     (* (- (py p1) (py p0)) (- (px p2) (px p0)))))

; Distance squared between points
(define (dist2 p1 p2)
  (let ((dx (- (px p1) (px p2)))
        (dy (- (py p1) (py p2))))
    (+ (* dx dx) (* dy dy))))

; Find lowest point (smallest y, then smallest x)
(define (find-lowest points)
  (foldl (lambda (best p)
           (cond ((< (py p) (py best)) p)
                 ((and (= (py p) (py best)) (< (px p) (px best))) p)
                 (t best)))
         (car points) (cdr points)))

; Remove element from list
(define (remove-elem x lst)
  (cond ((eq lst nil) nil)
        ((equal (car lst) x) (cdr lst))
        (t (cons (car lst) (remove-elem x (cdr lst))))))

; ---------- Graham Scan ----------
; Sort points by polar angle from pivot, then by distance
(define (polar-sort points pivot)
  (sort (lambda (a b) (polar-lt a b pivot)) points))

(define (polar-lt a b pivot)
  (let ((c (cross pivot a b)))
    (cond ((> c 0) t)         ; b is CCW of a: a sorts first
          ((< c 0) nil)       ; b is CW of a: b sorts first
          (t (< (dist2 pivot a) (dist2 pivot b))))))

; Build convex hull using Graham Scan
(define (graham-scan points)
  (let* ((pivot (find-lowest points))
         (rest (remove-elem pivot points))
         (sorted (polar-sort rest pivot))
         (stack (list pivot)))
    (graham-build stack sorted)))

(define (graham-build stack rest)
  (cond ((eq rest nil) (reverse stack))
        (t (let ((p (car rest)))
             (cond ((< (length stack) 2)
                    (graham-build (cons p stack) (cdr rest)))
                   (t (let ((second (car stack))
                            (first (cadr stack)))
                        (cond ((> (cross first second p) 0)
                               (graham-build (cons p stack) (cdr rest)))
                              (t (graham-build (cdr stack) rest))))))))))

; ---------- Jarvis March (Gift Wrapping) ----------
(define (jarvis-march points)
  (let ((start (find-lowest points)))
    (jarvis-wrap points start start nil)))

(define (jarvis-wrap points start current hull)
  (let ((next (find-next-hull points current)))
    (cond ((equal next start) (cons next hull))
          (t (jarvis-wrap points start next (cons next hull))))))

(define (find-next-hull points current)
  (foldl (lambda (candidate p)
           (cond ((equal p current) candidate)
                 ((equal candidate current) p)
                 (t (let ((c (cross current candidate p)))
                      (cond ((< c 0) p)
                            ((> c 0) candidate)
                            (t (cond ((> (dist2 current p) (dist2 current candidate)) p)
                                     (t candidate))))))))
         current points))

; ---------- Tests ----------
(define points
  (list (list 0 3) (list 2 2) (list 1 1) (list 2 1)
        (list 3 0) (list 0 0) (list 3 3)))

(define gh (graham-scan points))
(print "Graham Scan hull:" gh)

(define jh (jarvis-march points))
(print "Jarvis March hull:" jh)

; Expected hull in CCW order: (0,0) (3,0) (3,3) (0,3)
(define expected (list (list 0 0) (list 3 0) (list 3 3) (list 0 3)))

(check "graham-hull-size-4" (= (length gh) 4))
(check "jarvis-hull-size-4" (= (length jh) 4))

; Verify hull contains expected points (order may vary for Jarvis)
(check "graham-has-0-0" (any (lambda (p) (equal p (list 0 0))) gh))
(check "graham-has-3-0" (any (lambda (p) (equal p (list 3 0))) gh))
(check "graham-has-3-3" (any (lambda (p) (equal p (list 3 3))) gh))
(check "graham-has-0-3" (any (lambda (p) (equal p (list 0 3))) gh))
(check "jarvis-has-all-expected"
  (and (any (lambda (p) (equal p (list 0 0))) jh)
       (any (lambda (p) (equal p (list 3 0))) jh)
       (any (lambda (p) (equal p (list 3 3))) jh)
       (any (lambda (p) (equal p (list 0 3))) jh)))

; Verify Graham Scan produces 4-point hull with CCW orientation
(check "graham-hull4-ccw"
  (and (> (cross (nth 0 gh) (nth 1 gh) (nth 2 gh)) 0)
       (> (cross (nth 1 gh) (nth 2 gh) (nth 3 gh)) 0)
       (> (cross (nth 2 gh) (nth 3 gh) (nth 0 gh)) 0)
       (> (cross (nth 3 gh) (nth 0 gh) (nth 1 gh)) 0)))

; Both hulls should agree on point set (same number of unique points)
(check "graham-jarvis-agree-size" (= (length gh) (length jh)))

; Test on a single triangle
(define tri (list (list 0 0) (list 1 0) (list 0 1)))
(define tri-hull (graham-scan tri))
(check "triangle-hull-size-3" (= (length tri-hull) 3))

(print "=== Convex Hull Done ===")
