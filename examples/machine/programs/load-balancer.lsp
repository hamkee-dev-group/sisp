; Load balancer: round-robin dispatch with health checks, sticky sessions, failure detection.
; Backend: (id port healthy fail-count).  Demonstrates 3 backends, 10 requests, failover.

(define pool
  '((backend-1 8000 t 0)
    (backend-2 8001 t 0)
    (backend-3 8002 t 0)))

(define rr-index 0)
(define session-map nil)
(define health-counter 0)

(define (lb-backend id)
  (find (lambda (b) (equal (car b) id)) pool))

(define (lb-update-backend id port healthy fail-count)
  (setq pool
    (cons (list id port healthy fail-count)
          (remove (lambda (b) (equal (car b) id)) pool))))

(define (lb-set-healthy id state)
  (let ((b (lb-backend id)))
    (lb-update-backend id (cadr b) state (nth 3 b))))

(define (lb-inc-fails id)
  (let ((b (lb-backend id)))
    (let ((new-count (+ (nth 3 b) 1)))
      (lb-update-backend id (cadr b)
        (cond ((>= new-count 3) nil) (t (nth 2 b)))
        new-count))))

(define (lb-reset-fails id)
  (let ((b (lb-backend id)))
    (lb-update-backend id (cadr b) t 0)))

(define (lb-healthy-list)
  (map car
       (filter (lambda (b) (nth 2 b)) pool)))

(define (lb-round-robin)
  (let ((healthy (lb-healthy-list)))
    (cond ((null healthy) nil)
          (t (let* ((n (length healthy))
                    (i (mod rr-index n))
                    (selected (nth i healthy)))
               (setq rr-index (+ rr-index 1))
               selected)))))

(define (lb-sticky-backend client-ip)
  (let ((entry (find (lambda (e) (equal (car e) client-ip)) session-map)))
    (cond ((eq entry nil) nil)
          (t (cadr entry)))))

(define (lb-sticky-set client-ip backend-id)
  (setq session-map
    (cons (list client-ip backend-id)
          (remove (lambda (e) (equal (car e) client-ip)) session-map))))

(define (lb-dispatch client-ip)
  (let ((sticky (lb-sticky-backend client-ip)))
    (cond ((not (null sticky))
           (let ((b (lb-backend sticky)))
             (cond ((and (not (null b)) (nth 2 b))
                    (progn (print (cat "  sticky -> " (str-id sticky)))
                           sticky))
                   (t (progn (lb-sticky-set client-ip nil)
                             (lb-dispatch client-ip))))))
          (t (let ((selected (lb-round-robin)))
               (cond ((null selected)
                      (print "  NO HEALTHY BACKENDS")
                      nil)
                     (t (progn (lb-sticky-set client-ip selected)
                               (print (cat "  rr -> " (str-id selected)))
                               selected))))))))

(define (lb-simulate-request client-ip)
  (let ((backend (lb-dispatch client-ip)))
    (cond ((null backend) nil)
          (t (let ((ok (lb-probe backend)))
               (cond (ok (lb-reset-fails backend))
                     (t (lb-inc-fails backend)))
               ok)))))

(define (lb-probe backend-id)
  (let ((b (lb-backend backend-id)))
    (cond ((null b) nil)
          ((nth 2 b) t)
          (t nil))))

(define (lb-health-check)
  (setq health-counter (+ health-counter 1))
  (print (cat "--- Health check " (num-to-str health-counter)))
  (foreach (lambda (b)
             (let* ((id (car b))
                    (healthy (nth 2 b))
                    (fails (nth 3 b)))
               (print (cat (cat (cat (cat (cat "  " (str-id id))
                                        " healthy=")
                                 (cond (healthy "yes") (t "no ")))
                          " fails=")
                      (num-to-str fails)))))
           pool))

(define (lb-fail-backend id)
  (print (cat ">>> Simulating failure on " (str-id id)))
  (lb-set-healthy id nil)
  (lb-inc-fails id)
  (lb-inc-fails id)
  (lb-inc-fails id))

(define (str-id sym)
  (cond ((eq sym 'backend-1) "backend-1")
        ((eq sym 'backend-2) "backend-2")
        ((eq sym 'backend-3) "backend-3")
        (t "?")))

(define (make-ip i)
  (cat "192.168.1." (num-to-str i)))

; ---- demonstration ----
(print "=== LOAD BALANCER ===")

(print "")
(lb-health-check)

(print "")
(print "-- 10 requests distributed across 3 backends: --")
(lb-simulate-request (make-ip 1))
(lb-simulate-request (make-ip 2))
(lb-simulate-request (make-ip 3))
(lb-simulate-request (make-ip 4))
(lb-simulate-request (make-ip 5))
(lb-simulate-request (make-ip 6))
(lb-simulate-request (make-ip 7))
(lb-simulate-request (make-ip 8))
(lb-simulate-request (make-ip 9))
(lb-simulate-request (make-ip 10))

(print "")
(print "-- Same client repeats (sticky session): --")
(lb-simulate-request (make-ip 1))
(lb-simulate-request (make-ip 1))
(lb-simulate-request (make-ip 1))

(print "")
(print "-- Fail backend-2 (3 consecutive failures -> removed): --")
(lb-fail-backend 'backend-2)
(lb-health-check)

(print "")
(print "-- 5 more requests, traffic shifts to 1,3 only: --")
(lb-simulate-request (make-ip 11))
(lb-simulate-request (make-ip 12))
(lb-simulate-request (make-ip 13))
(lb-simulate-request (make-ip 14))
(lb-simulate-request (make-ip 15))

(print "")
(print "-- Sticky client for backend-2 re-routed to healthy backend: --")
(lb-simulate-request (make-ip 2))
