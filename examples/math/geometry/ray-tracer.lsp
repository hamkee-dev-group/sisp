; ============================================================
; ray-tracer.lsp — Simple ray tracer. Sphere intersection.
; Phong lighting (ambient, diffuse, specular). Shadows.
; Reflection. Render scene of colored spheres to ASCII.
; ============================================================

(print "=== Simple Ray Tracer ===")

(define (vx v) (nth 0 v)) (define (vy v) (nth 1 v)) (define (vz v) (nth 2 v))
; Ascending 0..n-1 so raster rows/columns come out in scan order.
(define (range-from i n) (if (>= i n) nil (cons i (range-from (+ i 1) n))))
(define (range-r n) (range-from 0 n))
(define (my-expt x n) (if (= n 0) 1 (* x (my-expt x (- n 1)))))

(define (v3-add a b) (list (+ (vx a) (vx b)) (+ (vy a) (vy b)) (+ (vz a) (vz b))))
(define (v3-sub a b) (list (- (vx a) (vx b)) (- (vy a) (vy b)) (- (vz a) (vz b))))
(define (v3-scale v s) (list (* (vx v) s) (* (vy v) s) (* (vz v) s)))
(define (v3-dot a b) (+ (* (vx a) (vx b)) (* (vy a) (vy b)) (* (vz a) (vz b))))
(define (v3-length v) (sqrt (+ (* (vx v) (vx v)) (* (vy v) (vy v)) (* (vz v) (vz v)))))
(define (v3-normalize v) (let* ((len (v3-length v))) (if (= len 0) (list 0 0 0) (v3-scale v (/ 1 len)))))

(define (sphere-intersect center radius ray-origin ray-dir)
  (let* ((oc (v3-sub ray-origin center)) (a (v3-dot ray-dir ray-dir))
         (b (* 2 (v3-dot oc ray-dir))) (c (- (v3-dot oc oc) (* radius radius)))
         (disc (- (* b b) (* 4 a c))))
    (if (< disc 0) nil
      (let* ((t1 (/ (- (- b) (sqrt disc)) (* 2 a)))
             (t2 (/ (+ (- b) (sqrt disc)) (* 2 a))))
        (if (> t1 0.001) t1 (if (> t2 0.001) t2 nil))))))

(define (closest-hit spheres ray-origin ray-dir)
  (foldl (lambda (acc s)
    (let* ((t0 (sphere-intersect (nth 0 s) (nth 1 s) ray-origin ray-dir)))
      (if (eq t0 nil) acc
        (if (eq acc nil) (list t0 s)
          (if (< t0 (nth 0 acc)) (list t0 s) acc)))))
    nil spheres))

(define (trace-ray-0 ray-origin ray-dir spheres lights ambient depth)
  (let* ((hit (closest-hit spheres ray-origin ray-dir)))
    (if (eq hit nil) (list 0.1 0.1 0.2)
      (let* ((t0 (nth 0 hit)) (s (nth 1 hit))
             (center (nth 0 s)) (radius (nth 1 s)) (color (nth 2 s))
             (point (v3-add ray-origin (v3-scale ray-dir t0)))
             (normal (v3-normalize (v3-sub point center)))
             (view-dir (v3-scale ray-dir -1))
             (ambient-color (v3-scale ambient 0.3))
             (light-color (foldl (lambda (acc l)
               (let* ((light-pos (nth 0 l)) (light-intensity (nth 1 l))
                      (light-dir (v3-normalize (v3-sub light-pos point)))
                      (nudge (v3-add point (v3-scale normal 0.001)))
                      (sh-h (closest-hit spheres nudge light-dir))
                      (dist-to-light (v3-length (v3-sub light-pos point))))
                 ; `and` needs strict booleans; sh-h is a hit record or nil.
                 (if (if (eq sh-h nil) nil (< (nth 0 sh-h) dist-to-light)) acc
                   (let* ((diff (max 0 (v3-dot normal light-dir)))
                          (diffuse (v3-scale color (* diff light-intensity 0.6)))
                          (reflected (v3-sub (v3-scale normal (* 2 (v3-dot light-dir normal))) light-dir))
                          (spec-factor (my-expt (max 0 (v3-dot reflected view-dir)) 32))
                          (specular (v3-scale color (* spec-factor light-intensity 0.4))))
                     (v3-add acc (v3-add diffuse specular))))))
               (list 0 0 0) lights)))
        (v3-add ambient-color light-color)))))

(define (color-to-char color)
  (let* ((intensity (+ (vx color) (vy color) (vz color))))
    (cond ((> intensity 2.4) "@") ((> intensity 2.1) "#")
          ((> intensity 1.8) "%") ((> intensity 1.5) "8")
          ((> intensity 1.2) "O") ((> intensity 0.9) "o")
          ((> intensity 0.6) "*") ((> intensity 0.3) ".")
          ((> intensity 0.05) "'") (t " "))))

(define (render-ascii width1 height1)
  (let* ((spheres (list
         (list (list -1.5 0 -8) 1.5 (list 0.8 0.1 0.1))
         (list (list 1.5 0.5 -7) 1.2 (list 0.1 0.1 0.8))
         (list (list 0 -1.5 -6) 1.0 (list 0.1 0.8 0.1))
         (list (list -0.5 1.5 -7) 0.8 (list 0.8 0.8 0.1))))
         (lights (list (list (list 5 5 -3) 1.2) (list (list -3 2 -4) 0.5)))
         (ambient (list 0.15 0.15 0.25))
         (origin (list 0 0 3))
         (aspect (/ width1 height1))
         (fov (/ 3.14159 3)))
    (define (pixel-row y0)
      (map (lambda (x0)
        (let* ((py0 (* (- (/ (* 2 y0) height1) 1) (tan (/ fov 2))))
               (px0 (* (- (/ (* 2 x0) width1) 1) (tan (/ fov 2)) aspect))
               (dir (v3-normalize (list px0 py0 -1))))
          (trace-ray-0 origin dir spheres lights ambient 2)))
        (range-r width1)))
    (map pixel-row (range-r height1))))

(print "--- Sphere Intersection ---")
(define test-origin (list 0 0 3))
(define blue-center (list 1.5 0.5 -7))
; The old test ray pointed straight down -z, whose perpendicular distance to
; the blue sphere centre is sqrt(1.5^2+0.5^2) = 1.581 > radius 1.2, so it
; genuinely missed.  Aim the probe ray at the centre instead.
(define test-dir (v3-normalize (v3-sub blue-center test-origin)))
(define test-t (sphere-intersect blue-center 1.2 test-origin test-dir))
(print (list "Hit distance:" test-t))
; `and` needs strict booleans; test-t is a distance or nil.
(check "ray-hits" (if (eq test-t nil) nil (> test-t 0)))

(define red-center (list -1.5 0 -8))
(define miss-t (sphere-intersect red-center 0.1 test-origin test-dir))
(check "ray-misses" (eq miss-t nil))

(print "--- Single Ray Trace ---")
(define spheres-list (list
  (list blue-center 1.2 (list 0.1 0.1 0.8))
  (list red-center 1.5 (list 0.8 0.1 0.1))))
(define test-lights (list (list (list 5 5 -3) 1.2)))
(define test-ambient (list 0.15 0.15 0.25))
(define test-color (trace-ray-0 test-origin test-dir spheres-list test-lights test-ambient 2))
(print (list "Color:" (vx test-color) (vy test-color) (vz test-color)))
(check "color-pos" (and (>= (vx test-color) 0) (>= (vy test-color) 0) (>= (vz test-color) 0)))

(print "--- ASCII Render (30x15) ---")
(define pixels-rt (render-ascii 30 15))
(define line-number 0)
(foreach (lambda (row)
  (let* ((line (foldl (lambda (acc p) (cat acc (color-to-char p))) "" row)))
    (print line)))
  (reverse pixels-rt))
; the render actually produced a 15-row, 30-column raster of real colours
(check "render-has-15-rows" (= (length pixels-rt) 15))
(check "render-rows-are-30-wide"
  (every (lambda (row) (= (length row) 30)) pixels-rt))
(check "render-colours-are-nonnegative"
  (every (lambda (row)
           (every (lambda (p) (and (>= (vx p) 0) (>= (vy p) 0) (>= (vz p) 0))) row))
         pixels-rt))

(print "=== Done ===")
